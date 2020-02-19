//set CORS to call "stocks" package on public server
ocpu.seturl("http://localhost:5656/ocpu/library/stats/R")

//some example data
//to run with different data, edit and press Run at the top of the page
var mydata = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];



var req = ocpu.rpc("sd",{
    x : mydata
}, function(output){
    alert("Standard Deviation equals: " + output);
});

//optional
req.fail(function(){
    alert("R returned an error: " + req.responseText);
});
