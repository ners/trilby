{ lib, ... }:

lib.trilbyUser {
  uid = 1000;
  name = "$username";
  initialHashedPassword = "$initialHashedPassword";
}
