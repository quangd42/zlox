const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

const Obj = @import("obj.zig");
const Value = @import("value.zig").Value;
const VM = @import("vm.zig");

const Entry = struct {
    key: ?*Obj.String, // Entry with null key is a tombstone
    value: Value,
};

const MAX_LOAD_PERCENTAGE = 75;

pub const Table = @This();

count: usize = 0,
entries: []?Entry,

/// A Table containing no entries.
pub const empty: Table = .{ .entries = &[_]?Entry{} };

pub fn deinit(self: *Table, gpa: Allocator) void {
    gpa.free(self.entries);
    self.* = undefined;
}

/// returns true if a new entry was added, false otherwise
pub fn set(self: *Table, gpa: Allocator, key: *Obj.String, value: Value) !bool {
    if (self.count + 1 > self.entries.len * MAX_LOAD_PERCENTAGE / 100) {
        try self.increaseCapacity(gpa);
    }
    const entry = findEntry(self.entries, key);
    const is_new = entry.* == null;
    if (is_new) self.count += 1;
    entry.* = .{ .key = key, .value = value };
    return is_new;
}

pub fn addAll(to: *Table, gpa: Allocator, from: *Table) !void {
    for (from.entries) |mb_entry| {
        const entry = mb_entry orelse continue;
        if (entry.key) |k| _ = try to.set(gpa, k, entry.value);
    }
}

pub fn get(self: *Table, key: *Obj.String) ?Value {
    if (self.entries.len == 0) return null;
    const entry = findEntry(self.entries, key);
    return if (entry.*) |e| e.value else null;
}

pub fn delete(self: *Table, key: *Obj.String) bool {
    if (self.entries.len == 0) return false;
    const entry = findEntry(self.entries, key);
    // if entry is empty or is a tombstone, no op
    if (entry.* == null or entry.*.?.key == null) return false;
    // else turn entry into tombstone
    entry.*.?.key = null;
    return true;
}

fn increaseCapacity(self: *Table, gpa: Allocator) !void {
    const new_cap = if (self.entries.len < 8) 8 else self.entries.len * 2;
    const entries = try gpa.alloc(?Entry, new_cap);
    // initialize slice for ease of access later
    for (entries) |*entry| entry.* = null;

    // redistribute existing entries
    self.count = 0;
    for (self.entries) |entry| {
        if (entry == null or entry.?.key == null) continue;
        const dest = findEntry(entries, entry.?.key.?);
        dest.* = entry;
        self.count += 1;
    }

    gpa.free(self.entries);
    self.entries = entries;
}

fn findEntry(entries: []?Entry, key: *Obj.String) *?Entry {
    var idx = key.hash & (entries.len - 1);
    var tombstone: ?*?Entry = null;
    while (true) : (idx = (idx + 1) & (entries.len - 1)) {
        const entry = &entries[idx];
        // empty entry is the end of the chain
        // return captured tombstone or itself if no tombstone
        const e = entry.* orelse return if (tombstone) |ts| ts else entry;
        const k = e.key orelse {
            // found tombstone, save it if this is first tombstone encountered
            if (tombstone == null) tombstone = entry;
            continue;
        };
        if (k == key) return entry; // found matching entry
    }
}

pub fn findString(self: *Table, chars: []const u8, hash: u32) ?*Obj.String {
    const entries = self.entries;
    if (entries.len == 0) return null;
    var idx = hash & (entries.len - 1);
    while (true) : (idx = (idx + 1) & (entries.len - 1)) {
        const entry = entries[idx];
        const e = entry orelse return null;
        const key = e.key orelse continue;
        // std.mem.eql already compares len
        if (key.hash == hash and std.mem.eql(u8, key.chars, chars)) return key;
    }
}

test "table ops" {
    var vm = try VM.init(testing.allocator);
    const gpa = vm.allocator;
    defer vm.deinit();
    var table: Table = .empty;
    defer table.deinit(gpa);
    const key1 = try Obj.String.init(vm, "key");
    const key2 = try Obj.String.init(vm, "not");
    // get from empty table
    var got = table.get(key1);
    try testing.expectEqual(null, got);
    // delete from empty table
    var deleted = table.delete(key1);
    try testing.expectEqual(false, deleted);
    // set key
    const set_key = try table.set(gpa, key1, .from(true));
    try testing.expect(set_key);
    try testing.expectEqual(1, table.count);
    // get back
    got = table.get(key1);
    try testing.expect(got != null);
    try testing.expectEqual(Value.from(true), got.?);
    // delete invalid key
    deleted = table.delete(key2);
    try testing.expect(!deleted);
    try testing.expectEqual(1, table.count);
    // delete valid key
    deleted = table.delete(key1);
    try testing.expect(deleted);
    // count should remain 1 because of tombstone
    try testing.expectEqual(1, table.count);
    // force increaseCap to clear tombstone, expect count = 0
    try table.increaseCapacity(gpa);
    try testing.expectEqual(0, table.count);
    // addAll
    var table2: Table = .empty;
    try table2.addAll(gpa, &table);
    for (table2.entries) |mb_entry| {
        const entry = mb_entry orelse continue;
        if (entry.key) |k| {
            const src = table.get(k);
            try testing.expect(src != null);
            try testing.expect(entry.value.eql(src.?));
        }
    }
    for (table.entries) |mb_entry| {
        const entry = mb_entry orelse continue;
        if (entry.key) |k| {
            const src = table2.get(k);
            try testing.expect(src != null);
            try testing.expect(entry.value.eql(src.?));
        }
    }
}

test "rewrite key" {
    var vm = try VM.init(testing.allocator);
    const gpa = vm.allocator;
    defer vm.deinit();
    const key1 = try Obj.String.init(vm, "hello world");
    const key2 = try Obj.String.init(vm, "hello ");
    const key3 = try Obj.String.init(vm, "world");
    const key4 = try key2.concat(vm, key3);
    const current_count = vm.globals.count;
    var set_key = try vm.globals.set(gpa, key1, .from(1));
    try testing.expect(set_key);
    try testing.expectEqual(current_count + 1, vm.globals.count);
    set_key = try vm.globals.set(gpa, key4, .from(true));
    try testing.expect(!set_key);
    try testing.expectEqual(current_count + 1, vm.globals.count);
    var got = vm.globals.get(key1);
    try testing.expect(got != null);
    try testing.expectEqual(Value.from(true), got.?);
    got = vm.globals.get(key4);
    try testing.expect(got != null);
    try testing.expectEqual(Value.from(true), got.?);
}
