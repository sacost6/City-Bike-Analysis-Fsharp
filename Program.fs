//
// F# program to analyze Divvy daily ride data.
//
// << YOUR NAME HERE >>
// U. of Illinois, Chicago
// CS 341, Fall 2019
// Project #04
//

#light

module project04

//
// ParseLine and ParseInput
//
// Given a sequence of strings representing Divvy data, 
// parses the strings and returns a list of lists.  Each
// sub-list denotes one bike ride.  Example:
//
//   [ [1522,141,17,5,1124]; ... ]
//
// The values are station id (from), station id (to), bike
// id, starting hour (0..23), starting day of week (0 Sunday-6 Saturday)
// and trip duration (secs), 
//
let ParseLine (line:string) = 
  let tokens = line.Split(',')
  let ints = Array.map System.Int32.Parse tokens
  Array.toList ints

let rec ParseInput lines = 
  let rides = Seq.map ParseLine lines
  Seq.toList rides
  
// This portion of functions helps with parsing bike info 
let Helper line =
  let BikeID = List.nth line 2
  BikeID
  
let RawBikeIDList list = 
  let A = List.empty 
  List.map Helper list |> List.append <| A
  
let GetBikeID list = 
  let A = List.empty 
  let M = List.map Helper list |> List.append <| A
  List.distinct M
  
let GetNumberBikes list = 
  let M = GetBikeID list
  printfn "# of bikes: %A" (List.length M)
  printfn ""
  
let rec GetBikeIDCount M chosenID = 
  match M with 
  | [] -> 0
  | head::tail -> 
    if head = chosenID then 1 + (GetBikeIDCount tail chosenID)
    else (GetBikeIDCount tail chosenID)

let GetBikeUsage list= 
  let M = RawBikeIDList list
  let counter = 0
  printf "BikeID> "
  let BikeChosen = System.Console.ReadLine()
  let chosenID = System.Int32.Parse BikeChosen
  let count = (GetBikeIDCount M chosenID)
  printfn ""
  printfn "# of rides for BikeID %A: %A" chosenID count
  printfn ""
  chosenID

let GetTime line = 
  List.nth line 5
 

let rec GetBikeTimeUsage list chosenID =
  match list with 
  | [] -> 0
  | head::tail -> 
      if ((Helper head) = chosenID) then ((GetTime head) + (GetBikeTimeUsage tail chosenID))
      else (GetBikeTimeUsage tail chosenID)
    
let GetTotalTime list chosenID = 
  let total = GetBikeTimeUsage list chosenID
  let minutes = ((float total)/60.0)
  let minutes = int minutes
  let minutes = float minutes 
  let seconds = total % 60
  printfn "Total time spent riding BikeID %A: %A minutes %A seconds" chosenID (int minutes) (int seconds)
  printfn "" 
  
let GetAverageTime list chosenID =
  let total = GetBikeTimeUsage list chosenID 
  let M = RawBikeIDList list
  let count = GetBikeIDCount M chosenID
  let average = (total |> float) / (count |> float)
  printfn "Average time spent riding BikeID %A: %.2f seconds" chosenID average
  printfn ""
  
// This portion of functions helps with station info parsing operations

let GetStationIDLine line = 
  List.nth line 1
  
let RawStationIDList list = 
  let A = List.empty 
  let A = List.map GetStationIDLine list |> List.append <| A
  A

let GetStationIDLine2 line = 
  List.nth line 0

let RawStationIDList2 list = 
  let A = List.empty 
  let A = List.map GetStationIDLine2 list |> List.append <| A
  A

let GetStationID list = 
  let A = List.empty 
  let M = List.map GetStationIDLine list |> List.append <| A
  List.distinct M

let rec GetStationIDCount M chosenID = 
  match M with 
  | [] -> 0
  | head::tail -> 
    if (head = chosenID) then 1 + (GetStationIDCount tail chosenID)
    else (GetStationIDCount tail chosenID)

let GetStationUsage list chosenID = 
  let M = RawStationIDList list
  let counter = GetStationIDCount M chosenID
  printfn ""
  printfn "# of rides to StationID %A: %A" chosenID counter
  counter

let rec GetTotalStationTime list chosenID = 
  match list with 
  | [] -> 0
  | head::tail -> 
      if((GetStationIDLine head)=chosenID) then (GetTime head) + (GetTotalStationTime tail chosenID)
      else (GetTotalStationTime tail chosenID)
  
let GetAverageStationTime list chosenID = 
  let M = RawStationIDList list
  let denom = GetStationUsage list chosenID
  let totalSeconds = GetTotalStationTime list chosenID
  let averagetime = (totalSeconds |> float) / (denom |> float)
  printfn ""
  printfn "Average time spent on trips leading to StationID %A: %.2f seconds" chosenID averagetime
  printfn ""
  
// get trip number for each day
let GetTripDay line = 
  List.nth line 4
  
let GetRawDayList list =
  let A = List.empty
  List.map GetTripDay list |> List.append <| A
  
let rec GetDayCount daylist day = 
  match daylist with 
  | [] -> 0
  | head::tail -> 
      if (head = day) then 1 + (GetDayCount tail day)
      else (GetDayCount tail day)

let DayParser list day = 
  GetDayCount (GetRawDayList list) day
  
let rec PrintStars counter = 
  match counter with 
  | 0 -> printf ""
  | _ -> 
      printf "*"
      PrintStars (counter-1)
      
let rec CountStationTrips stationlist chosenID = 
  match stationlist with 
  | [] -> 0
  | head::tail -> 
      if (head = chosenID) then 1 + (CountStationTrips tail chosenID)
      else (CountStationTrips tail chosenID)

// top ten stations 
let CountStationUsage list chosenID = 
  let M = RawStationIDList list
  let counter = CountStationTrips M chosenID
  let R = [counter ; chosenID]
  R
  
let GetStationListNumber list = 
  let A = RawStationIDList list
  let A = List.distinct A
  A
  
let GetTopTen list = 
  let stations = GetStationListNumber list
  let stations = List.sort stations
  let count = List.length stations 
  let A = List.map (CountStationUsage list) <| stations
  A
  
let GetSecond list = 
  List.nth list 1
  
let sorthelper A = 
  let B = List.sortBy (fun (head::tail) -> -head) A
  
  B
 
let SortTopTenList list = 
  let A = GetTopTen list 
  let B = sorthelper A
  B
  
let PrintTopTen A = 
  let list = SortTopTenList A
  let One = List.nth list 0
  let Two = List.nth list 1
  let Three = List.nth list 2
  let Four = List.nth list 3
  let Five = List.nth list 4
  let Six = List.nth list 5
  let Seven = List.nth list 6
  let Eight = List.nth list 7
  let Nine = List.nth list 8
  let Ten = List.nth list 9

  let One1 = List.nth One 0
  let Two1 = List.nth Two 0
  let Three1 = List.nth Three 0
  let Four1 = List.nth Four 0
  let Five1 = List.nth Five 0
  let Six1 = List.nth Six 0
  let Seven1 = List.nth Seven 0
  let Eight1 = List.nth Eight 0
  let Nine1 = List.nth Nine 0
  let Ten1 = List.nth Ten 0
  
  let One2 = List.nth One 1
  let Two2 = List.nth Two 1
  let Three2 = List.nth Three 1
  let Four2 = List.nth Four 1
  let Five2 = List.nth Five 1
  let Six2 = List.nth Six 1
  let Seven2 = List.nth Seven 1
  let Eight2 = List.nth Eight 1
  let Nine2 = List.nth Nine 1
  let Ten2 = List.nth Ten 1
  
  printfn "# of rides to station %A: %A" One2 One1
  printfn "# of rides to station %A: %A" Two2 Two1
  printfn "# of rides to station %A: %A" Three2 Three1
  printfn "# of rides to station %A: %A" Four2 Four1
  printfn "# of rides to station %A: %A" Five2 Five1
  printfn "# of rides to station %A: %A" Six2 Six1
  printfn "# of rides to station %A: %A" Seven2 Seven1
  printfn "# of rides to station %A: %A" Eight2 Eight1
  printfn "# of rides to station %A: %A" Nine2 Nine1
  printfn "# of rides to station %A: %A" Ten2 Ten1
  
[<EntryPoint>]
let main argv =
  //
  // input file name, then input divvy ride data and build
  // a list of lists:
  //
  printf "filename> "
  let filename = System.Console.ReadLine()
  let contents = System.IO.File.ReadLines(filename)
  let ridedata = ParseInput contents

  //printfn "%A" ridedata
  let N = List.length ridedata
  printfn ""
  printfn "# of rides: %A" N
  printfn ""
  
  // Get the number of bikes 
  GetNumberBikes ridedata
  
  // Get a single Bike's info
  // First get the number of times bike was ridden by ID
  let chosenID = GetBikeUsage ridedata
  // Second get the amount of time the bike was ridden for
  GetTotalTime ridedata chosenID
  // Get average amount of time bike was ridden for 
  GetAverageTime ridedata chosenID
  
  // get the chosen station ID
  printf "StationID> "
  let stationID = System.Console.ReadLine()
  let stationID = System.Int32.Parse stationID
  // get average time of rides to station 
  GetAverageStationTime ridedata stationID
  
  // print days of week
  let Sunday = DayParser ridedata 0
  let Monday = DayParser ridedata 1
  let Tuesday = DayParser ridedata 2
  let Wednesday = DayParser ridedata 3
  let Thursday = DayParser ridedata 4
  let Friday = DayParser ridedata 5
  let Saturday = DayParser ridedata 6
  printfn "Number of Trips on Sunday: %A" Sunday
  printfn "Number of Trips on Monday: %A" Monday
  printfn "Number of Trips on Tuesday: %A" Tuesday
  printfn "Number of Trips on Wednesday: %A" Wednesday
  printfn "Number of Trips on Thursday: %A" Thursday
  printfn "Number of Trips on Friday: %A" Friday
  printfn "Number of Trips on Saturday: %A" Saturday
  
  printfn ""
  printf "0: " 
  (PrintStars (Sunday / 10)) 
  printfn " %A" Sunday 
  printf "1: " 
  (PrintStars (Monday / 10)) 
  printfn " %A" Monday 
  printf "2: " 
  (PrintStars (Tuesday / 10)) 
  printfn " %A" Tuesday 
  printf "3: " 
  (PrintStars (Wednesday / 10)) 
  printfn " %A" Wednesday 
  printf "4: " 
  (PrintStars (Thursday / 10))
  printfn " %A" Thursday 
  printf "5: " 
  (PrintStars (Friday / 10))
  printfn " %A" Friday 
  printf "6: " 
  (PrintStars (Saturday / 10)) 
  printfn " %A" Saturday 
  printfn ""
  
  
  PrintTopTen ridedata
  printfn ""
  
  0 
