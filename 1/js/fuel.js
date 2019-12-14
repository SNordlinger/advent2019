const fs = require('fs');

function getFuelReq(mass) {
  const fuelReq =  Math.floor(mass / 3) - 2;
  if (fuelReq <= 0) {
    return 0;
  } else {
    return fuelReq + getFuelReq(fuelReq);
  }
}

function getInput() {
  const fileStr = fs.readFileSync('input.txt',
    {encoding: 'utf8'});
  const strList = fileStr.trim().split('\n');
  return strList.map((s) => parseInt(s, 10))
}

let totalFuel = 0;
getInput().forEach(function (mass) {
  totalFuel += getFuelReq(mass);
});

console.log(totalFuel);
