# Validators for elegant-STATE configuration

{
  ValidPort = std.contract.from_predicate (fun x =>
    std.is_number x && x >= 1 && x <= 65535
  ),

  ValidPercentage = std.contract.from_predicate (fun x =>
    std.is_number x && x >= 0.0 && x <= 1.0
  ),

  NonEmpty = std.contract.from_predicate (fun x =>
    std.is_string x && std.string.length x > 0
  ),

  ValidPath = std.contract.from_predicate (fun x =>
    std.is_string x && std.string.length x > 0
  ),

  PositiveNumber = std.contract.from_predicate (fun x =>
    std.is_number x && x > 0
  ),

  NonNegativeNumber = std.contract.from_predicate (fun x =>
    std.is_number x && x >= 0
  ),
}
