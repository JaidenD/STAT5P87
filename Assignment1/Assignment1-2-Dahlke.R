data = read.csv("~/STAT5P87/Assignment1/a1-q2-data.csv")

unique_id = 0

for(school in unique(data$school)){
  # Filter rows for current school
  school_rows = (data$school == school)
  
  # Determine the unique students in the current school
  unique_students = unique(data$student[school_rows])
  
  # Map old student IDs to new student IDs
  student_mapping = setNames(seq(unique_id+1, unique_id + length(unique_students)), unique_students)
  
  # Update the student column for current school
  data$student[school_rows] = student_mapping[as.character(data$student[school_rows])]
  
  # Update counter
  unique_id = max(student_mapping)
}


data

















