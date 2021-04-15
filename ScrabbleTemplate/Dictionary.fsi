// Insert your Dictionary.fsi file here. All modules must be internal

module internal Dictionary

    type Dict

    val empty : unit -> Dict
    val insert : string -> Dict -> Dict
    val step : char -> Dict -> (bool * Dict) option
    //val reverse : Dict -> (bool * Dict) option