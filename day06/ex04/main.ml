let () =
  Random.self_init () ;
  let rec createListPeople (i:int) (res:People.people list) = match i with
      | 0 -> res
      | _ -> createListPeople (i - 1) ((new People.people "Dave") :: res)
  in
  let rec createListDoctors (i:int) (peopleList:People.people list) (res:Doctor.doctor list) = match i with
      | 0 -> res
      | _ -> createListDoctors (i - 1) peopleList (new Doctor.doctor  :: res)
  in
  let rec createListDaleks (i:int) res = match i with
      | 0 -> res
      | _ -> createListDaleks (i - 1) ((new Dalek.dalek) :: res)
  in
  let lstPeople = createListPeople ((Random.int 15) + 3) [] in
  let lstDoctors = createListDoctors ((Random.int 15) + 3) lstPeople [] in
  let lstDaleks = createListDaleks ((Random.int 15) + 3) [] in
  let armyPeople = new Army.army lstPeople in
  let armyDoctors = new Army.army lstDoctors in
  let armyDaleks = new Army.army lstDaleks in
  let g = new Galifrey.galifrey armyDaleks armyDoctors armyPeople in
  g#do_time_war
