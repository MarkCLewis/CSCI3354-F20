package basics

object RandomStudent extends App {
    val students = Array(
        "Kenny", "Ren", "Arjun", "Aidan", "Quentin", "Nick", "Lucy", "Chet"
    )
    println(students(util.Random.nextInt(students.length)))
}