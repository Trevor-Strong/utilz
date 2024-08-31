//! `bool` like enums using common names for the `true` and `false` values

pub const Enabled = enum { disable, enable };

pub const Allowed = enum { disallowed, allowed };
pub const AllowDeny = enum { deny, allow };

pub const YesNo = enum { no, yes };
