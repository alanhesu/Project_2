#!/usr/bin/env perl

local $addr=0;
local $lnum=0;
local %mem;
%opmap = (
    EQ    => {FMT=> "RD,RS1,RS2",  IWORD=>"000000 RS1  RS2  RD   00 001000"},
    LT    => {FMT=> "RD,RS1,RS2",  IWORD=>"000000 RS1  RS2  RD   00 001001"},
    LE    => {FMT=> "RD,RS1,RS2",  IWORD=>"000000 RS1  RS2  RD   00 001010"},
    NE    => {FMT=> "RD,RS1,RS2",  IWORD=>"000000 RS1  RS2  RD   00 001011"},
    ADD   => {FMT=> "RD,RS1,RS2",  IWORD=>"000000 RS1  RS2  RD   00 100000"},
    SUB   => {FMT=> "RD,RS1,RS2",  IWORD=>"000000 RS1  RS2  RD   00 101000"},
    AND   => {FMT=> "RD,RS1,RS2",  IWORD=>"000000 RS1  RS2  RD   00 100100"},
    OR    => {FMT=> "RD,RS1,RS2",  IWORD=>"000000 RS1  RS2  RD   00 100101"},
    XOR   => {FMT=> "RD,RS1,RS2",  IWORD=>"000000 RS1  RS2  RD   00 100110"},
    NAND  => {FMT=> "RD,RS1,RS2",  IWORD=>"000000 RS1  RS2  RD   00 101100"},
    NOR   => {FMT=> "RD,RS1,RS2",  IWORD=>"000000 RS1  RS2  RD   00 101101"},
    NXOR  => {FMT=> "RD,RS1,RS2",  IWORD=>"000000 RS1  RS2  RD   00 101110"},
    LB    => {FMT=> "RD,Imm(RS1)", IWORD=>"010000 RS1  RD              Imm"},
    LH    => {FMT=> "RD,Imm(RS1)", IWORD=>"010001 RS1  RD              Imm"},
    LW    => {FMT=> "RD,Imm(RS1)", IWORD=>"010010 RS1  RD              Imm"},
    SB    => {FMT=> "RS2,Imm(RS1)",IWORD=>"011000 RS1  RS2             Imm"},
    SH    => {FMT=> "RS2,Imm(RS1)",IWORD=>"011101 RS1  RS2             Imm"},
    SW    => {FMT=> "RS2,Imm(RS1)",IWORD=>"011010 RS1  RS2             Imm"},
    BEQ   => {FMT=> "RS1,RS2,Imm", IWORD=>"001000 RS1  RS2           PCRel"},
    BLT   => {FMT=> "RS1,RS2,Imm", IWORD=>"001001 RS1  RS2           PCRel"},
    BLE   => {FMT=> "RS1,RS2,Imm", IWORD=>"001010 RS1  RS2           PCRel"},
    BNE   => {FMT=> "RS1,RS2,Imm", IWORD=>"001011 RS1  RS2           PCRel"},
    JAL   => {FMT=> "RD,Imm(RS1)", IWORD=>"001100 RS1  RD            ShImm"},
    ADDI  => {FMT=> "RD,RS1,Imm",  IWORD=>"100000 RS1  RD              Imm"},
    ANDI  => {FMT=> "RD,RS1,Imm",  IWORD=>"100100 RS1  RD              Imm"},
    ORI   => {FMT=> "RD,RS1,Imm",  IWORD=>"100101 RS1  RD              Imm"},
    XORI  => {FMT=> "RD,RS1,Imm",  IWORD=>"100110 RS1  RD              Imm"},
	
    # NOT is implemented using XORI
	NOT    => {FMT=>"RD,RS",      ITEXT=>["NAND RD,RS,RS"]},
	# Variants of JAL
	CALL   => {FMT=>"Imm\\\(RS1\\\)",ITEXT=>["JAL  RA,Imm(RS1)"]},
	RET    => {FMT=>"",              ITEXT=>["JAL  R6,0(RA)"]},
	JMP    => {FMT=>"Imm\\\(RS1\\\)",ITEXT=>["JAL  R6,Imm(RS1)"]},
	# Condition-equivalent branches and comparisons
	BGT    => {FMT=>"RS1,RS2,Imm",ITEXT=>["BLT RS2,RS1,Imm"]},
	BGE    => {FMT=>"RS1,RS2,Imm",ITEXT=>["BLE RS2,RS1,Imm"]},
	GT     => {FMT=>"RD,RS1,RS2",ITEXT=>["LT RD,RS2,RS1"]},
	GE     => {FMT=>"RD,RS1,RS2",ITEXT=>["LE RD,RS2,RS1"]},
	# SUBI is implemented using ADDI
	SUBI   => {FMT=>"RD,RS,Imm",  ITEXT=>["ADDI RD,RS,-Imm"]},
    # B is implemented using BEQ
    BR     => {FMT=>"Imm",        ITEXT=>["BEQ  ZERO,ZERO,Imm"]},
    );

sub RmWs{
  my $inp=$_[0];
  $inp=~s/\s//g;
  return $inp;
}
# Size of the memory/register word (in bits)
$WordSize=32;
#Size of the instruction word
$InstSize=$WordSize;
# Size of the immediate field
$ImmSize=14;
%srnum =(
    SCS => "000",
    SIH => "001",
    SRA => "010",
    SII => "011",
    SR0 => "110",
    SR1 => "111",
    );

$RegNumSize=6;
%RegNames = ();
for(my $rnum=0;$rnum<64;$rnum++){
	$RegNames{$rnum}=[sprintf("R%d",$rnum)];
}
push $RegNames{0},"ZERO";
push $RegNames{1},"RV";
push $RegNames{2},"RA";
push $RegNames{3},"SP";
push $RegNames{4},"GP";
push $RegNames{5},"FP";
for(my $rnum=0;$rnum<16;$rnum++){
	push $RegNames{16+$rnum},sprintf("A%d",$rnum);
	push $RegNames{32+$rnum},sprintf("T%d",$rnum);
	push $RegNames{48+$rnum},sprintf("S%d",$rnum);
}
# Returns a string of $bits binary digits that corresponds to $num
sub IntToBinStr{
  my ($num,$bits)=($_[0],$_[1]);
  my $binstr="";
  for(my $i=0;$i<$bits;$i++){
    $binstr=(($num&1)?"1":"0").$binstr;
	$num=($num>>1);
  }
  return $binstr;
}
# RegNums maps a register name to the corresponding bit-string 
%RegNums = ();
sub InitRegnums{
  print "In regnums now\n";
  foreach my $rnum ( keys %RegNames ){
	foreach my $rnam ( @{$RegNames{$rnum}} ){
	  $RegNums{$rnam}=&IntToBinStr($rnum,$RegNumSize);
	}
  }
}
&InitRegnums();

sub ToBin{
  my ($num,$bits,$wbits,$lnum)=($_[0],$_[1],$_[2],$_[3]);
  print "ToBin $num $bits $wbits $lnum\n";
  my $str="";
  my $curNum=int($num)&((1<<$wbits)-1);
  my $negRef=int(-1)&((1<<$wbits)-1);
  my $lastBit=0;
  for(my $bit=0;$bit<$bits;$bit++){
    $lastBit=($curNum&1);
    $str=$lastBit.$str;
    $curNum=$curNum>>1;
	$negRef=$negRef>>1;
  }
  ((($lastBit==0)&&($curNum==0))||(($lastBit==1)&&($curNum==$negRef)))
    or die "Line $lnum: Immediate operand $num too large to fit in $bits bits\n";
  return $str;
}

sub AsmInst{
   my ($op,$args,$pc,$line,$lnum)=($_[0],&RmWs($_[1]),$_[2],$_[3],$_[4]);
   my $sfmt=$opmap{$op}{FMT};
   my $mfmt=$opmap{$op}{IWORD};
   # Parse $args
   my ($argtail,$fmttail)=($args,$sfmt);
   while(true){
      (($argtail ne "") and ($fmttail ne ""))
         or last;
      $argtail=~/^([^\,\(\)]+)([\s\,\(\)]*)(.*)$/
         or die "Line $lnum: Invalid instruction format at $argtail\n";
      my $argnow=$1;
      my $argsep=$2;
      $argtail=$3;
      $fmttail=~/^([^\,\(\)]+)([\s\,\(\)]*)(.*)$/
         or die "Internal error at line $lnum: invalid FMT $fmttail for opcode $op\n";
      my $fmtnow=$1;
      my $fmtsep=$2;
      $fmttail=$3;
      if($fmtnow=~/^RD|RS1|RS2$/){
         # Arg should be a register name
         (exists $RegNums{$argnow})
            or die "Line $lnum: Invalid register name $argnow\n";
         $mfmt=~s/$fmtnow/$RegNums{$argnow}/g;
      }elsif($fmtnow=~/^SRD|SRS$/){
         # Arg should be a register name
         (exists $srnum{$argnow})
            or die "Line $lnum: Invalid system register name $argnow\n";
         $mfmt=~s/$fmtnow/$srnum{$argnow}/g;
      }elsif($fmtnow=~/^Imm$/){
         # Arg should be a label or constant
         my $imm=&LblGet($argnow);
         ($imm ne "") or
            $imm=&NumGet($argnow);
         ($imm ne "") or
            die "Line $lnum: Invalid immediate operand $argnow\n";
         # Take care of various uses of the immediate operand
         if($mfmt=~/ShImm/){
            my $immVal=&ToBin($imm/4,$ImmSize,$WordSize,$lnum);
            $mfmt=~s/ShImm/$immVal/eg;
         }elsif($mfmt=~/ImmHi/){
            my $immVal=&ToBin($imm>>16,$ImmSize,$WordSize,$lnum);
            $mfmt=~s/ImmHi/$immVal/eg;
         }elsif($mfmt=~/Imm/){
            my $immVal=&ToBin($imm,$ImmSize,$WordSize,$lnum);
            $mfmt=~s/Imm/$immVal/eg;
         }elsif($mfmt=~/PCRel/){
           my $relVal=&ToBin(($imm-($pc+4))/4,$ImmSize,$WordSize,$lnum);
           $mfmt=~s/PCRel/$relVal/eg;
         }
      }else{
         die "Internal error at line $lnum: invalid FMT $fmtnow for opcode $op\n";
      }
      ($argsep eq $fmtsep) or
         die "Line $lnum: Invalid syntax, expected $fmtsep but got $argsep\n";
   }
   print "Result: $op $args => $mfmt\n";
   # Remove spaces from the instruction word binary string
   $mfmt=~s/ //g;
   ($mfmt=~/^[01]{$InstSize}$/) or
      die "Internal error at line $lnum: IWORD fields not present in FMT for opcode $op\n"; 
   my $iword=0;
   while($mfmt ne ""){
     ($mfmt=~/^([01])(.*)$/) or
       die "Internal error at line $lnum: Non-binary result after FMT->IWORD substitution for opcode $op\n";
     $iword=$iword*2+int($1);
     $mfmt=$2;
   }
   print "Res: $iword\n";
   &SetMem($pc,$iword,$line,$lnum);
}

sub SubInst{
  my ($op,$args,$substfmt,$pc,$line,$lnum)=($_[0],$_[1],$_[2],$_[3],$_[4],$_[5]);
  my $argsfmt=$opmap{$op}{FMT};
  my @arglist=split(/\s*,\s*/,$args);
  my @fmtlist=split(/\s*,\s*/,$argsfmt);
  (@arglist == @fmtlist) or
    die "Line $lnum: Invalid number of arguments for pseudo-instruction $op\n"; 
  for(my $i=0;$i<@arglist;$i++){
	$substfmt=~s/$fmtlist[$i]/$arglist[$i]/g;
  }
  ($substfmt=~/^\s*([^\s]+)\s*(.*)$/) or
    die "Internal error at line $lnum: Pseudo-instruction translates to poorly-formatted instruction $line\n";
  my $opi=$1;
  my $argi=$2;
  (exists $opmap{$opi}) or
    die "Internal error at line $lnum: Pseudo-instruction translates to invalid opcode $opi\n";
  (exists $opmap{$opi}{IWORD}) or
    die "Internal error at line $lnum: Pseudo-instruction translates to pseudo-instruction $opi\n";
  &AsmInst($opi,$argi,$pc,$substfmt,$lnum);
}

local $lblfmt="[A-Z][0-9A-Z_]+";
my %lbl=();
sub LblGet{
  my $lname=$_[0];
  (exists $lbl{$lname})
    or return "";
  return $lbl{$lname};
}

my $numfmt="0X[0-9A-F]+|-?[0-9]+";
sub NumGet{
  my $numstr=$_[0];
  my $retVal;
  ($numstr=~/^($numfmt)$/) or
      return "";
  if(substr($numstr,0,2) eq "0X"){
    $retVal=hex(substr($numstr,2));
  }else{
    $retVal=int($numstr);
  }
  if($retVal&(1<<($WordSize-1))){
	$retVal|=((-1)^((1<<($WordSize-1))-1));
  }
  return $retVal;
}

sub SetMem{
  my ($addr,$val,$line,$lnum)=($_[0],$_[1],$_[2],$_[3]);
  (! exists $mem{$addr})
    or die "Line $lnum: Memory address $addr already filled\n";
  print "Set mem[$addr] to $val\n";
  $mem{$addr}=$val;
  $text{$addr}=$line;
}

my $InpFileName="Sorter2.a32";
my $OutFileName="Sorter2.mif";
if(@ARGV == 2){
  $InpFileName=$ARGV[0];
  $OutFileName=$ARGV[1];
}

for(my $pass=1;$pass<=2;$pass++){
print "PASS $pass START\n";
open(InFile,"< $InpFileName")
  or die "Can't open input file $InpFileName";
for(my $lnum=1;<InFile>;$lnum++){
  $line=uc($_);
  chomp($line);
  # Remove comment from the end of the line
  if($line=~/^([^;]*);.*$/){
    $line=$1;
  }
  if($line=~/^\s*$/){
    # Empty line
  }elsif($line=~/^\s*\.ORIG\s+($numfmt)\s*$/){
    $addr=&NumGet($1);
  }elsif($line=~/^\s*\.NAME\s+($lblfmt)\s*=\s*($numfmt)\s*$/){
    if($pass==1){
      (! exists $lbl{$1})
        or die "Line $lnum: Label $1 redefined";
    }else{
      (&LblGet($1) == &NumGet($2))
        or die "Line $lnum: Label $1 is pass 2 not the same as in pass 1\n";
    }
    $lbl{$1}=&NumGet($2);
    print "Pass $pass: Label $1 set to $lbl{$1}\n";
  }elsif($line=~/^\s*\.WORD\s+($numfmt|$lblfmt)\s*$/){
    if($pass==2){
      my $str=$1;
      if($str=~/^($numfmt)$/){
	my $num=&NumGet($str);
        ($num ne "") or
	  die "Line $lnum: Invalid numeric constant $str\n";
	&SetMem($addr,$num,$line,$lnum);
      }else{
	my $lbl=&LblGet($str);
        ($lbl ne "") or
	  die "Line $lnum: Label $str not defined\n";
        &SetMem($addr,$lbl,$line,$lnum);
      }
    }
    $addr+=4;
  }elsif($line=~/^\s*($lblfmt):\s*$/){
    if($pass==1){
      (! exists $lbl{$1})
        or die "Line $lnum: Label $1 redefined";
    }else{
      (&LblGet($1) == $addr)
        or die "Line $lnum: Label $1 is pass 2 not the same as in pass 1\n";
    }
    print "Pass $pass: Label $1 set to $addr\n";
    $lbl{$1}=$addr;
  }elsif($line=~/^\s*([^\s]+)\s*(.*)$/){
    my $opcode=$1;
    my $args=$2;
    (exists $opmap{$opcode})
      or die "Line $lnum: Invalid opcode $opcode\n";
    if(exists $opmap{$opcode}{IWORD}){
      ($pass==1) or
        &AsmInst($opcode,$args,$addr,$line,$lnum);
      $addr+=4;
    }elsif(exists $opmap{$opcode}{ITEXT}){
      my $expRef=$opmap{$opcode}{ITEXT};
      foreach my $subi ( @{$expRef} ){
        ($pass==1) or
		  &SubInst($opcode,$args,$subi,$addr,$line,$lnum);
		$addr+=4;
      }
    }else{
      die "Internal error: Incorrect opmap definition for opcode $opcode!\n";
    }
  }else{
    die "Don't know what line $line is supposed to do!\n";
  }
}
close(InFile);
print "PASS $pass DONE\n";
}

my $MemWords=(1<<14);
my $AddrGran=1;
open(OutFile,"> $OutFileName")
  or die "Can't open output file $OutFileName";
print OutFile "WIDTH=$InstSize;\n";
print OutFile "DEPTH=$MemWords;\n";
print OutFile "ADDRESS_RADIX=HEX;\n";
print OutFile "DATA_RADIX=HEX;\n";
print OutFile "CONTENT BEGIN\n";
my $FillStart=0;
for(my $i=0;$i<$MemWords;$i++){
  if(exists $mem{$i*($WordSize/(8*$AddrGran))}){
    if($FillStart<$i){
      printf(OutFile "[%08x..%08x] : DEAD;\n",$FillStart,$i-1);
    }
    printf(OutFile "-- @ 0x%08x :",$i*($WordSize/(8*$AddrGran)));
    printf(OutFile " %s\n",$text{$i*($WordSize/(8*$AddrGran))});
    printf(OutFile "%08x : %08x;\n",$i,$mem{$i*($WordSize/(8*$AddrGran))});
    $FillStart=$i+1;
  }
}
if($FillStart<$MemWords){
  printf(OutFile "[%04x..%04x] : DEAD;\n",$FillStart,$MemWords-1);
}
print OutFile "END;\n";
close(OutFile);
