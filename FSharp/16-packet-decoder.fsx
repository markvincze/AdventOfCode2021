open System
open System.IO

type PacketInfo = {
    version : int
    typeId : int
}

type Packet =
| Literal of PacketInfo * int64
| Operator of PacketInfo * (Packet list)

let toBinary (hex : string) = 
    let binary = Array.create (hex.Length * 4) false
    for i in 0..hex.Length-1 do
        let c = hex.[i]
        let binStr = Convert.ToString(Convert.ToInt32(c.ToString(), 16), 2).PadLeft(4, '0')
        binary.[i * 4] <- binStr.[0] = '1'
        binary.[i * 4 + 1] <- binStr.[1] = '1'
        binary.[i * 4 + 2] <- binStr.[2] = '1'
        binary.[i * 4 + 3] <- binStr.[3] = '1'
    binary

let toString binary =
    String.Join("", binary |> Array.map (fun b -> if b then "1" else "0"))

let number (binary : bool[]) from length =
    [ from..from+length-1 ]
    |> List.fold
           (fun acc index -> acc + (if binary.[index]
                                    then Math.Pow(2.0, from + length - 1 - index |> float) |> int
                                    else 0))
           0

let literal (binary : bool[]) from =
    let rec literal (binary : bool[]) from acc bits =
        let marker = binary.[from]
        let num = number binary (from + 1) 4 |> int64

        if marker
        then literal binary (from + 5) ((acc + num) <<< 4) (bits + 5)
        else (acc + num), bits + 5

    literal binary from 0L 0

let binary = File.ReadAllText "16-packet-decoder-input.txt"
             |> toBinary

let rec parsePacketsForBitlength binary from bitLength =
    let rec parsePacketsForBitlength binary from bitLength packets totalBits =
        let packet, bits = parsePacket binary from
        if bits >= bitLength
        then packet :: packets, totalBits + bits
        else parsePacketsForBitlength binary (from + bits) (bitLength - bits) (packet :: packets) (totalBits + bits)
    
    parsePacketsForBitlength binary from bitLength [] 0
and parseNPackets binary from n =
    let rec parseNPackets binary from n packets totalBits =
        let packet, bits = parsePacket binary from
        if n = 1
        then packet :: packets, totalBits + bits
        else parseNPackets binary (from + bits) (n - 1) (packet :: packets) (totalBits + bits)
    
    parseNPackets binary from n [] 0
and parsePacket binary from =
    let version = number binary from 3
    let typeId = number binary (from + 3) 3

    if typeId = 4
    then let num, bits = (literal binary (from + 6)) 
         Literal ({ version = version; typeId = typeId}, int64 num), bits + 6
    else let lengthTypeId = binary.[from + 6]
         if lengthTypeId
         then let packetCount = number binary (from + 6 + 1) 11
              let packets, bits = parseNPackets binary (from + 6 + 1 + 11) packetCount
              Operator ({ version = version; typeId = typeId}, packets), 6 + 1 + 11 + bits
         else let bitLength = number binary (from + 6 + 1) 15
              let packets, bits = parsePacketsForBitlength binary (from + 6 + 1 + 15) bitLength
              Operator ({ version = version; typeId = typeId}, packets), 6 + 1 + 15 + bits

let rec versionNumberSum packet =
    match packet with
    | Literal ({ version = v}, _) -> v
    | Operator ({ version = v}, packets) -> v + List.sumBy versionNumberSum packets

let packet, _ = parsePacket binary 0

let result1 = packet |> versionNumberSum

let rec calculateValue packet =
    match packet with
    | Literal (_, v) -> v
    | Operator ({ typeId = typeId }, subPackets) ->
        match typeId with
        | 0 -> subPackets |> List.sumBy calculateValue
        | 1 -> subPackets |> List.fold (fun acc p -> acc * (calculateValue p)) 1L
        | 2 -> subPackets |> List.map calculateValue |> List.min
        | 3 -> subPackets |> List.map calculateValue |> List.max
        | 5 -> let [ p1; p2 ] = subPackets
               if (calculateValue p2) > (calculateValue p1) then 1L else 0L
        | 6 -> let [ p1; p2 ] = subPackets
               if (calculateValue p2) < (calculateValue p1) then 1L else 0L
        | 7 -> let [ p1; p2 ] = subPackets
               if (calculateValue p1) = (calculateValue p2) then 1L else 0L
        | _ -> failwith "Invalid typeId"

let result2 = packet |> calculateValue

//2457453848
//1631421724