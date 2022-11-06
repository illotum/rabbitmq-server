## This Source Code Form is subject to the terms of the Mozilla Public
## License, v. 2.0. If a copy of the MPL was not distributed with this
## file, You can obtain one at https://mozilla.org/MPL/2.0/.
##
## Copyright (c) 2007-2020 VMware, Inc. or its affiliates.  All rights reserved.

defmodule InfoKeysTest do
  import RabbitMQ.CLI.Ctl.InfoKeys

  use ExUnit.Case, async: false
  import TestHelper

  test "prepare translates aliases" do
    assert prepare_info_keys(["apple"], %{apple: :banana}) == [:banana]
  end

  test "prepare works without aliases" do
    assert prepare_info_keys(["apple"], %{}) == [:apple]
    assert prepare_info_keys(["apple"]) == [:apple]
  end

  test "validate translates aliases" do
    assert validate_info_keys(["apple"], ["apple"], %{apple: :banana}) == {:ok, [:banana]}
  end

  test "validate works without aliases" do
    assert validate_info_keys(["apple"], ["apple"], %{}) == {:ok, [:apple]}
    assert validate_info_keys(["apple"], ["apple"]) == {:ok, [:apple]}
  end

  test "with_valid translates aliases" do
    assert with_valid_info_keys(["apple"], ["apple"], %{apple: :banana}, fn v -> v end) == [:banana]
  end

  test "with_valid works without aliases" do
    assert with_valid_info_keys(["apple"], ["apple"], %{}, fn v -> v end) == [:apple]
    assert with_valid_info_keys(["apple"], ["apple"], fn v -> v end) == [:apple]
  end
end
