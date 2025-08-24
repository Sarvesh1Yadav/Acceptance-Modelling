MU_Merit_calculator = function(subject,GPA){
  if(subject == "Arts and Sciences"){
    if(GPA>=0 & GPA<= 3.17){
      MU_SCH = 24500;
    }else if(GPA > 3.17 & GPA <=3.45){
      MU_SCH = 26000;
    }else if(GPA > 3.45 & GPA <=3.62){
      MU_SCH = 27000;
    }else if(GPA > 3.62 & GPA <=3.78){
      MU_SCH = 29000;
    }else if(GPA > 3.78 & GPA <= 4.00){
      MU_SCH = 30000;
    }
    else{
      MU_SCH = 0;
    }
  }
if(subject == "Business Administration"){
    if(GPA>=0 & GPA<= 3.14){
      MU_SCH = 25000;
    }else if(GPA > 3.14 & GPA <=3.35){
      MU_SCH = 26500;
    }else if(GPA > 3.35 & GPA <=3.52){
      MU_SCH = 27500;
    }else if(GPA > 3.52 & GPA <= 3.67){
      MU_SCH = 28500;
    }else if(GPA > 3.67 & GPA <= 4.00){
      MU_SCH = 29500;
    }
  else{
    MU_SCH = 0;
  }
  }
  if(subject == "Communication"){
    if(GPA>=0 & GPA<= 3.22){
      MU_SCH = 24500;
    }else if(GPA > 3.22 & GPA <=3.46){
      MU_SCH = 25500;
    }else if(GPA > 3.46 & GPA <=3.75){
      MU_SCH = 27000;
    }else if(GPA > 3.75 & GPA <= 3.90){
      MU_SCH = 29000;
    }else if(GPA > 3.90 & GPA <= 4.00){
      MU_SCH = 30000;
    }
    else{
      MU_SCH = 0;
    }
  }
  if(subject == "Education"){
    if(GPA>=0 & GPA<= 3.24){
      MU_SCH = 24500;
    }else if(GPA > 3.24 & GPA <=3.52){
      MU_SCH = 25000;
    }else if(GPA > 3.52 & GPA <=3.69){
      MU_SCH = 27000;
    }else if(GPA > 3.69 & GPA <= 3.85){
      MU_SCH = 29000;
    }else if(GPA > 3.85 & GPA <= 4.00){
      MU_SCH = 30000;
    }
    else{
      MU_SCH = 0;
    }
  }
  if(subject == "Engineering"){
    if(GPA>=0 & GPA<= 3.24){
      MU_SCH = 25000;
    }else if(GPA > 3.24 & GPA <=3.47){
      MU_SCH = 26000;
    }else if(GPA > 3.47 & GPA <=3.62){
      MU_SCH = 27500;
    }else if(GPA > 3.62 & GPA <= 3.78){
      MU_SCH = 29000;
    }else if(GPA > 3.78 & GPA <= 4.00){
      MU_SCH = 30000;
    }
    else{
      MU_SCH = 0;
    }
  }
  if(subject == "Health Sciences"){
    if(GPA>=0 & GPA<= 3.28){
      MU_SCH = 24500;
    }else if(GPA > 3.28 & GPA <=3.50){
      MU_SCH = 26000 ;
    }else if(GPA > 3.50 & GPA <=3.64){
      MU_SCH = 27000;
    }else if(GPA > 3.64 & GPA <= 3.76){
      MU_SCH = 28500;
    }else if(GPA > 3.76 & GPA <= 4.00){
      MU_SCH = 29000;
    }
    else{
      MU_SCH = 0;
    }
  }
  if(subject == "Nursing"){
    if(GPA>=0 & GPA<= 3.48){
      MU_SCH = 23000;
    }else if(GPA > 3.48 & GPA <=3.63){
      MU_SCH = 24000;
    }else if(GPA > 3.63 & GPA <=3.76){
      MU_SCH = 25000;
    }else if(GPA > 3.76 & GPA <= 3.88){
      MU_SCH = 26000;
    }else if(GPA > 3.88 & GPA <= 4.00){
      MU_SCH = 27500;
    }
    else{
      MU_SCH = 0;
    }
  }
  return(MU_SCH)
}
