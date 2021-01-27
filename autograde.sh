#!/bin/bash

rm -f _user_output
dotnet build
# echo 'divvy01.csv' | dotnet run > _user_output
dotnet run | tee _user_output

diff _expected_output _user_output
