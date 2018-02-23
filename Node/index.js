const readCSV = require('./readCSV')
// Harvard Network Dataset
let canvasNetworkDataverse = readCSV('/data/CNPC_1401-1509_DI_v1_1_2016-03-01.csv')

// Open University Learning Analytics dataset
let assessments = readCSV('/data/assessments.csv')
let courses = readCSV('/data/courses.csv')
let studentAssessment = readCSV('/data/studentAssessment.csv')
let studentInfo = readCSV('/data/assessments.csv')
let studentRegistration = readCSV('/data/studentRegistration.csv')
let vle = readCSV('/data/vle.csv')

// studentVle is very large (~500MB) and will need to be handled via streams.
// let studentVle = readCSV('/data/studentVle.csv')

const main = async () => {
  // uncomment the below line to use the Harvard Network Learning Analytics dataset
  // canvasNetworkDataverse = await canvasNetworkDataverse

  // uncomment the below line to use the Open University Learning Analytics dataset 
  // [assessments, courses, studentAssessment, studentInfo, studentRegistration, vle] = await Promise.all([assessments, courses, studentAssessment, studentInfo, studentRegistration, vle])
}

main()
