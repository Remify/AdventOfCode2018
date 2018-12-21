open System.Collections.Generic
let input = "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up"

type GuardEvent =
   | ShiftBegin of int
   | WakeUp of unit 
   | FallAsleep of unit
   | None 
   
let ParseMinute (line: string) = 
     let minute = line.[15..16]
     int minute

let ParseEvent (line: string): GuardEvent = 
    match line.[19..line.Length - 1] with 
       | txt when txt.Contains("wakes up") -> GuardEvent.WakeUp ()
       | txt when txt.Contains("falls asleep") -> GuardEvent.FallAsleep ()
       | txt when txt.Contains("Guard ") -> 
          let id = int (txt.[7..10].Trim())
          GuardEvent.ShiftBegin id
       | _ -> GuardEvent.None

let Solve (input: string) = 
    let r = input.Split '\n'

    let mutable guardId: int = 0
    let mutable sinceAsleep = 0
    let dict = new Dictionary<int, int []>();
    for line in r do
      let minute = line |> ParseMinute
      let event = line |> ParseEvent
      match event with 
       | ShiftBegin(id) -> 
            guardId <- id
            sinceAsleep <- 0
       | WakeUp() -> 
            let minutesSlept = [|sinceAsleep..minute - 1|]
            
            // TODO : add to Dict
            sinceAsleep <- -1
       | FallAsleep() ->
            sinceAsleep <- minute
    
