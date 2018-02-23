# Node package for the Hackathon-3.0

This Node package has everything you need to get started with exploring the Harvard Network Dataset and the Open University Learning Analytics dataset using Node.js.

## Getting Started

These instructions will get you set up with running the project. The csv files are imported as JSON objects to make manipulating them simple. 

### Prerequisites

1. **Install [Node 8.0.0 or greater](https://nodejs.org)**.
2. **Install [Git](https://git-scm.com/downloads)**. 

### Installing and Setup

1. First, clone this repo. `git clone https://github.com/ubc/LA-OD-Hackathon-3.0.git`
2. Then cd into the repo. `cd LA-OD-Hackathon-3.0/Node`
3. Run the installation script. `npm install` (If you see `babel-node: command not found`, you've missed this step.)
4. Place all the csv files in the `/data` folder. You will have to first unzip the Open University Learning Analytics dataset.
5. To use the Harvard Network Dataset, uncomment the line below in `index.js`.
```javascript
canvasNetworkDataverse = await canvasNetworkDataverse
```
6. To use the Open University Learning Analytics dataset, uncomment the line below in `index.js`
```javascript 
[assessments, courses, studentAssessment, studentInfo, studentRegistration, vle] = await Promise.all([assessments, courses, studentAssessment, studentInfo, studentRegistration, vle])
```
7. To run the script, use either `npm start` or `node index.js`.

## Authors

* **Justin Lee** 
(https://github.com/justin0022)

## License

This project is licensed under the MIT License.
