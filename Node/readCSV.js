const { createReadStream } = require('fs')
const parse = require('csv-parse')
const path = require('path')

const readCSV = filename => new Promise((resolve, reject) => {
  const options = { delimiter: ',', columns: true, relax: true, auto_parse: true }
  const parser = parse(options, (err, data) => {
    if (err) reject(err)
    else resolve(data)
  })
  createReadStream(path.join(__dirname, filename)).pipe(parser)
})

module.exports = readCSV
