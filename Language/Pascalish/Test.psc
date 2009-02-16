program AverageOfTwoNumbers
  var x, y : integer;
  var z : anytype;
  procedure getinput
    begin
      read x;
      read y;
    end
  procedure average
    begin
      z = x + y;
      z = z / 2;
    end
  procedure showresult
    var r, equalsign :  char;
    begin
      r = 82;         // ascii value for 'R'
      equalsign = 61; // ascii value for '='
      write r;
      write equalsign;
      write z;
    end
  begin
    call getinput;
    call average;
    call showresult;
  end
