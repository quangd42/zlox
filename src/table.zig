const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

const ObjString = @import("obj.zig").String;
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

const Entry = struct {
    // Tombstone has null key
    key: ?*ObjString,
    value: Value,
};

const MAX_LOAD = 0.75;

const Table = struct {
    count: usize = 0,
    entries: []?Entry,
    allocator: Allocator,

    const Self = @This();
    pub fn init(allocator: Allocator) Table {
        return Table{
            .entries = &[_]?Entry{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.entries);
    }

    pub fn set(self: *Self, key: *ObjString, value: Value) !bool {
        // ...yucky
        const new_count: f64 = @floatFromInt(@as(u64, self.count) + 1);
        const current_load = @as(f64, @floatFromInt(self.entries.len)) * @as(f64, MAX_LOAD);
        if (new_count > current_load) try self.increaseCapacity();
        const dest = findEntry(self.entries, key);
        const is_new = dest.* == null;
        dest.* = .{ .key = key, .value = value };
        if (is_new) self.count += 1;
        return is_new;
    }

    pub fn get(self: *Self, key: *ObjString) ?Value {
        if (self.entries.len == 0) return null;
        const entry = findEntry(self.entries, key);
        return if (entry.* != null) entry.*.?.value else null;
    }

    pub fn delete(self: *Self, key: *ObjString) bool {
        if (self.entries.len == 0) return false;
        const entry = findEntry(self.entries, key);
        // if entry is empty or is a tombstone, no op
        if (entry.* == null or entry.*.?.key == null) return false;
        // else turn entry into tombstone
        entry.*.?.key = null;
        return true;
    }

    fn increaseCapacity(self: *Self) !void {
        const new_cap = if (self.entries.len < 8) 8 else self.entries.len * 2;
        const entries = try self.allocator.alloc(?Entry, new_cap);
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

        self.allocator.free(self.entries);
        self.entries = entries;
    }

    fn findEntry(entries: []?Entry, key: *ObjString) *?Entry {
        var idx = key.hash % entries.len;
        var tombstone: ?*?Entry = null;
        while (true) : (idx = (idx + 1) % entries.len) {
            const entry = &entries[idx];
            if (entry.* == null) {
                // found empty entry, stop probing and return
                return if (tombstone != null) tombstone.? else entry;
            }
            if (entry.*.?.key == null) {
                // found tombstone, save it if this is first tombstone
                // encountered
                if (tombstone == null) tombstone = entry;
                continue;
            }
            // TODO: string interning
            if (entry.*.?.key.?.eql(key)) {
                // found matching entry
                return entry;
            }
        }
    }
};

test "table ops" {
    var vm = VM.init(testing.allocator);
    defer vm.deinit();
    var table = Table.init(testing.allocator);
    defer table.deinit();
    const key1 = try ObjString.init(&vm, "key");
    const key2 = try ObjString.init(&vm, "not");
    // get from empty table
    var got = table.get(key1);
    try testing.expectEqual(null, got);
    // delete from empty table
    var deleted = table.delete(key1);
    try testing.expectEqual(false, deleted);
    // set key
    const set = try table.set(key1, .{ .Bool = true });
    try testing.expect(set);
    try testing.expectEqual(1, table.count);
    // get back
    got = table.get(key1);
    try testing.expect(got != null);
    try testing.expectEqual(Value{ .Bool = true }, got.?);
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
    try table.increaseCapacity();
    try testing.expectEqual(0, table.count);
}
