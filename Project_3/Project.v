module Project(
	input        CLOCK_50,
	input        RESET_N,
	input  [3:0] KEY,
	input  [9:0] SW,
	output [6:0] HEX0,
	output [6:0] HEX1,
	output [6:0] HEX2,
	output [6:0] HEX3,
	output [6:0] HEX4,
	output [6:0] HEX5,
	output [9:0] LEDR
);

  parameter DBITS    =32;
  parameter INSTSIZE =32'd4;
  parameter INSTBITS =32;
  parameter REGNOBITS=6;
  parameter REGWORDS=(1<<REGNOBITS);
  parameter IMMBITS  =14;
  parameter STARTPC  =32'h100;
  parameter ADDRHEX  =32'hFFFFF000;
  parameter ADDRLEDR =32'hFFFFF020;
  parameter ADDRKEY  =32'hFFFFF080;
  parameter ADDRSW   =32'hFFFFF090;
  parameter ADDRTCNT =32'hFFFFF100;
  parameter ADDRTLIM =32'hFFFFF104;
  parameter ADDRTCTL =32'hFFFFF108;
  parameter IMEMINITFILE="TestNOPS.mif";
  parameter IMEMADDRBITS=16;
  parameter IMEMWORDBITS=2;
  parameter IMEMWORDS=(1<<(IMEMADDRBITS-IMEMWORDBITS));
  parameter DMEMADDRBITS=16;
  parameter DMEMWORDBITS=2;
  parameter DMEMWORDS=(1<<(DMEMADDRBITS-DMEMWORDBITS));
  parameter TEST=32'hFFFFFFFF;

  parameter OP1BITS=6;
  parameter OP1_ALUR =6'b000000;
  parameter OP1_BEQ  =6'b001000;
  parameter OP1_BLT  =6'b001001;
  parameter OP1_BLE  =6'b001010;
  parameter OP1_BNE  =6'b001011;
  parameter OP1_JAL  =6'b001100;
  parameter OP1_LW   =6'b010010;
  parameter OP1_SW   =OP1_LW+6'b001000;
  parameter OP1_ADDI =6'b100000;
  parameter OP1_ANDI =6'b100100;
  parameter OP1_ORI  =6'b100101;
  parameter OP1_XORI =6'b100110;

  parameter OP2BITS=6;
  parameter OP2_NOP  =6'b000000;
  parameter OP2_EQ   =OP1_BEQ;
  parameter OP2_LT   =OP1_BLT;
  parameter OP2_LE   =OP1_BLE;
  parameter OP2_NE   =OP1_BNE;
  parameter OP2_ADD  =OP1_ADDI;
  parameter OP2_AND  =OP1_ANDI;
  parameter OP2_OR   =OP1_ORI;
  parameter OP2_XOR  =OP1_XORI;
  parameter OP2_SUB  =OP2_ADD|6'b001000;
  parameter OP2_NAND =OP2_AND|6'b001000;
  parameter OP2_NOR  =OP2_OR |6'b001000;
  parameter OP2_NXOR =OP2_XOR|6'b001000;
  parameter OP2_JALR =OP1_JAL;

  // The reset signal comes from the reset button on the DE0-CV board
  // RESET_N is active-low, so we flip its value ("reset" is active-high)
  wire clk,locked;
  // The PLL is wired to produce clk and locked signals for our logic
  Pll myPll(
    .refclk(CLOCK_50),
	 .rst      (!RESET_N),
	 .outclk_0 (clk),
    .locked   (locked)
  );
  wire reset=!locked;

	// The PC register and update logic
	reg  [(DBITS-1):0] PC;
	always @(posedge clk) begin
	if(reset)
		PC<=STARTPC;
	else if(mispred_B)
		PC<=pcgood_B;
	else if(!stall_F)
		PC<=pcpred_F;
	end
	// This is the value of "incremented PC", computed in stage 1
	wire [(DBITS-1):0] pcplus_F=PC+INSTSIZE;
	// This is the predicted value of the PC
	// that we used to fetch the next instruction
	wire [(DBITS-1):0] pcpred_F=pcplus_F;

	// Instruction-fetch
	(* ram_init_file = IMEMINITFILE *)
	reg [(DBITS-1):0] imem[(IMEMWORDS-1):0];
	wire [(DBITS-1):0] inst_F=imem[PC[(IMEMADDRBITS-1):IMEMWORDBITS]];

  	// If fetch and decoding stages are the same stage,
	// just connect signals from fetch to decode
	// wire [(DBITS-1):0] inst_D=inst_F;
	// wire [(DBITS-1):0] pcplus_D=pcplus_F;
	// wire [(DBITS-1):0] pcpred_D=pcpred_F;
 //  wire [(DBITS-1):0] pcpred_A=pcpred_D;
	// Instruction decoding
	// These have zero delay from inst_D
	// because they are just new names for those signals
  wire [(DBITS-1):0] off_D;
	wire [(OP1BITS-1):0]   op1_D=inst_D[(DBITS-1):(DBITS-OP1BITS)];
	wire [(REGNOBITS-1):0] rs_D,rt_D,rd_D;
	assign {rs_D,rt_D,rd_D}=inst_D[(DBITS-OP1BITS-1):(DBITS-OP1BITS-3*REGNOBITS)];
	wire [(OP2BITS-1):0] op2_D=inst_D[(OP2BITS-1): 0];
	wire [(IMMBITS-1):0] rawimm_D=inst_D[(IMMBITS-1):0];
  SXT #(.IBITS(IMMBITS),.OBITS(DBITS)) sxt1(rawimm_D,off_D);

	// Register-read
	reg [(DBITS-1):0] regs[(REGWORDS-1):0];
	// Two read ports, always using rs and rt for register numbers
	wire [(REGNOBITS-1):0] rregno1_D=rs_D, rregno2_D=rt_D;
	wire [(DBITS-1):0] regval1_D=regs[rregno1_D];
	wire [(DBITS-1):0] regval2_D=regs[rregno2_D];
  // wire [(DBITS-1):0] regval2_A=regval2_D;

	// TODO: Get these signals to the ALU somehow
  reg aluimm_D;
  reg [(OP2BITS-1):0] alufunc_D;
  wire signed [(DBITS-1):0] aluin1_A=regval1_A;
  wire signed [(DBITS-1):0] aluin2_A=aluimm_A?off_A:regval2_A;
  // wire [(DBITS-1):0] aluin2_A=32'h12345678;

	reg signed [(DBITS-1):0] aluout_A;
	always @(alufunc_A or aluin1_A or aluin2_A)
	case(alufunc_A)
		OP2_EQ:  aluout_A={31'b0,aluin1_A==aluin2_A};
		OP2_LT:  aluout_A={31'b0,aluin1_A< aluin2_A};
		OP2_LE:  aluout_A={31'b0,aluin1_A<=aluin2_A};
		OP2_NE:  aluout_A={31'b0,aluin1_A!=aluin2_A};
		OP2_ADD: aluout_A=aluin1_A+aluin2_A;
		OP2_AND: aluout_A=aluin1_A&aluin2_A;
		OP2_OR:  aluout_A=aluin1_A|aluin2_A;
		OP2_XOR: aluout_A=aluin1_A^aluin2_A;
		OP2_SUB: aluout_A=aluin1_A-aluin2_A;
		OP2_NAND:aluout_A=~(aluin1_A&aluin2_A);
		OP2_NOR: aluout_A=~(aluin1_A|aluin2_A);
		OP2_NXOR:aluout_A=~(aluin1_A^aluin2_A);
		default: aluout_A={DBITS{1'bX}};
	endcase

	// TODO: Generate the dobranch, brtarg, isjump, and jmptarg signals somehow...
  reg isbranch_D;
  wire dobranch_A=isbranch_A&&aluout_A[0];
  wire [(DBITS-1):0] brtarg_A = pcplus_A + (off_A << 2);
  reg isjump_D;
  // wire isjump_A=isjump_D;
  wire [(DBITS-1):0] jmptarg_A=regval1_A+(off_A<<2);
  // TODO: fix jmptarg to get rs + 4*imm

  // wire [(DBITS-1):0] pcplus_A=pcplus_D;
  wire [(DBITS-1):0] pcgood_M=
  	dobranch_M?brtarg_M:
  	isjump_M?jmptarg_M:
  	pcplus_M;
	// wire mispred_A=(pcgood_A!=pcpred_A);
	// wire mispred_B=mispred_A&&!isnop_A;
  wire mispred_B=(pcgood_M!=pcpred_M)&&!isnop_M;
	wire [(DBITS-1):0] pcgood_B=pcgood_M;

	// TODO: This is a good place to generate the flush_? signals
  // TODO: Flush less often or increase clock frequency
  wire flush_F, flush_D, flush_A;
  assign flush_F=(dobranch_M || isjump_M);
  assign flush_D=flush_D;
  assign flush_A=flush_F;

  wire stall_F, stall_D;
  // wire stall_F=1'b0;
  assign stall_D=((rs_D == wregno_A) || (rs_D == wregno_M) || (rs_D == wregno_W))
                    || ((rt_D == wregno_A) || (rt_D == wregno_M) || (rt_D == wregno_W))
                    && (wrreg_A || wrreg_M || wrreg_W);
  assign stall_F=stall_D;

  // wire flush_D;
  // assign flush_D=wrreg_M&&((wregno_M==rs_D)
  //   ||(wregno_M==rt_D)
  //   ||(isjump_M));

	// TODO: Write code that produces wmemval_M, wrmem_M, wrreg_M, etc.
    reg wrmem_M, wrreg_W;
    reg [(DBITS-1):0] wmemval_M;
    wire [(DBITS-1):0] memaddr_M;
    always @(posedge clk or posedge reset)
        if(reset) begin
            wrreg_W<=1'b0;
            wrmem_M<=1'b0;
            wmemval_M<={DBITS{1'b0}};
        end
        else begin
            wrreg_W<=isnop_M?1'b0:wrreg_M;
            wrmem_M<=isnop_A?1'b0:wrmem_A;
            wmemval_M<=regval2_A;
        end

	// Create and connect HEX register
	reg [23:0] HexOut;
	SevenSeg ss5(.OUT(HEX5),.IN(HexOut[23:20]));
	SevenSeg ss4(.OUT(HEX4),.IN(HexOut[19:16]));
	SevenSeg ss3(.OUT(HEX3),.IN(HexOut[15:12]));
	SevenSeg ss2(.OUT(HEX2),.IN(HexOut[11:8]));
	SevenSeg ss1(.OUT(HEX1),.IN(HexOut[7:4]));
	SevenSeg ss0(.OUT(HEX0),.IN(HexOut[3:0]));
	always @(posedge clk or posedge reset)
		if(reset)
			HexOut<=24'h123456;
		else if(wrmem_M&&(memaddr_M==ADDRHEX))
			HexOut <= wmemval_M[23:0];
    /*
    else if(PC<32'h00000104)
      // HexOut<=inst_F[23:0];
      HexOut<=reghex[23:0];
    */

  reg [9:0] led;
  always @(posedge clk or posedge reset) begin
    if(reset)
      led<=10'b0;
    else if(wrmem_M&&(memaddr_M==ADDRLEDR))
      led<=wmemval_M[9:0];
    /*
    else begin
      led<=TEST[9:0];
    end
    */
  end
  assign LEDR=led;

	// TODO: Write the code for LEDR here

	// Now the real data memory
	wire MemEnable=!(memaddr_M[(DBITS-1):DMEMADDRBITS]);
	wire MemWE=(!reset)&wrmem_M&MemEnable;
	(* ram_init_file = IMEMINITFILE, ramstyle="no_rw_check" *)
	reg [(DBITS-1):0] dmem[(DMEMWORDS-1):0];
	always @(posedge clk)
		if(MemWE)
			dmem[abus[(DMEMADDRBITS-1):DMEMWORDBITS]]<=dbus;
	wire [(DBITS-1):0] MemVal=MemWE?{DBITS{1'bX}}:dmem[abus[(DMEMADDRBITS-1):DMEMWORDBITS]];
  assign dbus=(MemEnable&&!wrmem_M)?MemVal:{DBITS{1'bz}};
	// Connect memory and input devices to the bus

  wire [(DBITS-1):0] abus;
  tri [(DBITS-1):0] dbus;
  wire we;

  assign abus=memaddr_M;
  assign we=wrmem_M;
  assign dbus=wrmem_M?wmemval_M:{DBITS{1'bz}};

  // Timer #(.BITS(DBITS), .BASE(ADDRTCNT))
  //   timer(.ABUS(abus),.DBUS(dbus),.WE(we),.INTR(),.CLK(clk),.LOCK(locked),.RESET(reset),.DEBUG());
  Keys #(.BITS(DBITS), .BASE(ADDRKEY))
    keys(.ABUS(abus),.DBUS(dbus),.WE(we),.INTR(),.CLK(clk),.LOCK(locked),.RESET(reset),.DEBUG(),.KEY(KEY));
  Switches #(.BITS(DBITS), .BASE(ADDRSW))
    switches(.ABUS(abus),.DBUS(dbus),.WE(we),.INTR(),.CLK(clk),.LOCK(locked),.RESET(reset),.DEBUG(),.SW(SW));

	wire [(DBITS-1):0] memout_M=
		MemEnable?dbus:
		(memaddr_M==ADDRKEY)?dbus:
		(memaddr_M==ADDRSW)?dbus:
    (memaddr_M==ADDRHEX)?{8'b0,HexOut}:
    (memaddr_M==ADDRLEDR)?{22'b0,led}:
    ((memaddr_M==ADDRTCNT)||(memaddr_M==ADDRTLIM)||(memaddr_M==ADDRTCTL)&&!wrmem_M)?dbus:
		{DBITS{1'bz}};

	// TODO: Decide what gets written into the destination register (wregval_M),
	// when it gets written (wrreg_M) and to which register it gets written (wregno_M)
    reg [(REGNOBITS-1):0] wregno_W;
    always @(posedge clk or posedge reset)
        if(reset)
            wregno_W<={REGNOBITS{1'b0}};
        else
            wregno_W<=wregno_M;

    wire [(DBITS-1):0] wregval_W=
        selaluout_W?aluout_W:
        selmemout_W?memout_W:
        selpcplus_W?pcplus_W:
        {DBITS{1'b0}};

	always @(posedge clk)
		if(wrreg_W&&!reset)
			regs[wregno_W]<=wregval_W;

	// Decoding logic
  reg isnop_D, wrmem_D, selaluout_D, selmemout_D, selpcplus_D, wrreg_D;
  reg [(REGNOBITS-1):0] wregno_D;
	always @* begin
		{aluimm_D,      alufunc_D}=
		{    1'bX,{OP2BITS{1'bX}}};
		{isbranch_D,isjump_D,isnop_D,wrmem_D}=
		{      1'b0,    1'b0,   1'b0,   1'b0};
		{selaluout_D,selmemout_D,selpcplus_D,wregno_D,          wrreg_D}=
		{       1'bX,       1'bX,       1'bX,{REGNOBITS{1'bX}},   1'b0};
		if(reset|flush_D)
			isnop_D=1'b1;
		else case(op1_D)
		  OP1_ALUR:
        case(op2_D)
          OP2_NOP:
            {isnop_D}<=
            {1'b1};
          default:
      			{aluimm_D,alufunc_D,selaluout_D,selmemout_D,selpcplus_D,wregno_D,wrreg_D}=
      			{    1'b0,    op2_D,       1'b1,       1'b0,       1'b0,    rd_D,   1'b1};
        endcase


      OP1_ADDI,OP1_ANDI,OP1_ORI,OP1_XORI:
        {aluimm_D,alufunc_D,selaluout_D,selmemout_D,selpcplus_D,wregno_D,wrreg_D}=
        {1'b1,op1_D,1'b1,1'b0,1'b0,rt_D,1'b1};

      OP1_BEQ,OP1_BLT,OP1_BNE,OP1_BLE:
        {aluimm_D,alufunc_D,isbranch_D}=
        {1'b0,op1_D,1'b1};

      OP1_JAL:
        // {aluimm_D,alufunc_D,isjump_D,selaluout_D,selmemout_D,selpcplus_D,wregno_D,wrreg_D}=
        // {1'b1,OP2_ADD,1'b1,1'b0,1'b0,1'b1,rt_D,1'b1};
        {isjump_D,selaluout_D,selmemout_D,selpcplus_D,wregno_D,wrreg_D}=
        {1'b1,1'b0,1'b0,1'b1,rt_D,1'b1};

      OP1_LW:
        {aluimm_D,alufunc_D,selaluout_D,selmemout_D,selpcplus_D,wregno_D,wrreg_D}=
        {1'b1,OP2_ADD,1'b0,1'b1,1'b0,rt_D,1'b1};

      OP1_SW:
        {aluimm_D,alufunc_D,wrmem_D}=
        {1'b1,OP2_ADD,1'b1};
		default:  ;
		endcase
	end

    // F->D buffer
    reg [(DBITS-1):0] pcplus_D, inst_D, pcpred_D;
    always @(posedge clk or posedge reset)
      if(reset) begin
        {pcplus_D, inst_D, pcpred_D}<=
          {{DBITS{1'b0}}, {DBITS{1'b0}}, {DBITS{1'b0}}};
      end
      else if(!flush_F) begin
        {pcplus_D, inst_D, pcpred_D}<=
          {pcplus_F, inst_F, pcpred_F};
      end else begin
        {pcplus_D, inst_D, pcpred_D}<=
          {{DBITS{1'b0}}, {DBITS{1'b0}}, {DBITS{1'b0}}};
      end

    // D->A buffer
    reg [(DBITS-1):0] pcplus_A, regval1_A, regval2_A, off_A, pcpred_A;
    reg [(OP1BITS-1):0] op1_A, op2_A;
    reg [(REGNOBITS-1):0] rs_A, rt_A, rd_A;
    reg aluimm_A, isbranch_A, isjump_A, isnop_A, wrmem_A, selaluout_A, selmemout_A, selpcplus_A, wrreg_A;
    reg [(OP2BITS-1):0] alufunc_A;
    reg [(REGNOBITS-1):0] wregno_A;
    always @(posedge clk or posedge reset)
      if(reset) begin
        {pcplus_A, regval1_A, regval2_A, off_A, op1_A, op2_A, rs_A, rt_A, rd_A, pcpred_A}<=
          {{DBITS{1'b0}}, {DBITS{1'b0}}, {DBITS{1'b0}}, {DBITS{1'b0}}, {OP1BITS{1'b0}}, {OP2BITS{1'b0}}, {REGNOBITS{1'b0}}, {REGNOBITS{1'b0}}, {REGNOBITS{1'b0}}, {DBITS{1'b0}}};
        {aluimm_A, isbranch_A, isjump_A, isnop_A, wrmem_A, selaluout_A, selmemout_A, selpcplus_A, wrreg_A}<=
          {9'b0};
        {alufunc_A, wregno_A}<=
          {{OP2BITS{1'b0}}, {REGNOBITS{1'b0}}};
      end
      else if(!stall_D && !flush_D) begin
        {pcplus_A, regval1_A, regval2_A, off_A, op1_A, op2_A, rs_A, rt_A, rd_A, pcpred_A}<=
          {pcplus_D, regval1_D, regval2_D, off_D, op1_D, op2_D, rs_D, rt_D, rd_D, pcpred_D};
        {aluimm_A, isbranch_A, isjump_A, isnop_A, wrmem_A, selaluout_A, selmemout_A, selpcplus_A, wrreg_A}<=
          {aluimm_D, isbranch_D, isjump_D, isnop_D, wrmem_D, selaluout_D, selmemout_D, selpcplus_D, wrreg_D};
        {alufunc_A, wregno_A}<=
          {alufunc_D, wregno_D};
      end else begin
        {pcplus_A, regval1_A, regval2_A, off_A, op1_A, op2_A, rs_A, rt_A, rd_A, pcpred_A}<=
          {{DBITS{1'b0}}, {DBITS{1'b0}}, {DBITS{1'b0}}, {DBITS{1'b0}}, {OP1BITS{1'b0}}, {OP2BITS{1'b0}}, {REGNOBITS{1'b0}}, {REGNOBITS{1'b0}}, {REGNOBITS{1'b0}}, {DBITS{1'b0}}};
        {aluimm_A, isbranch_A, isjump_A, isnop_A, wrmem_A, selaluout_A, selmemout_A, selpcplus_A, wrreg_A}<=
          {9'b0};
        {alufunc_A, wregno_A}<=
          {{OP2BITS{1'b0}}, {REGNOBITS{1'b0}}};
      end

    // A->M buffer
    reg [(DBITS-1):0] pcplus_M, brtarg_M, jmptarg_M, aluout_M, pcpred_M;
    reg aluimm_M, isbranch_M, isjump_M, isnop_M, selaluout_M, selmemout_M, selpcplus_M, dobranch_M, wrreg_M;
    reg [(REGNOBITS-1):0] wregno_M;
    assign memaddr_M=aluout_M;
    always @(posedge clk or posedge reset)
      if(reset) begin
        {pcplus_M, brtarg_M, jmptarg_M, aluout_M, pcpred_M}<=
          {{DBITS{1'b0}}, {DBITS{1'b0}}, {DBITS{1'b0}}, {DBITS{1'b0}}, {DBITS{1'b0}}};
        {aluimm_M, isbranch_M, isjump_M, isnop_M, selaluout_M, selmemout_M, selpcplus_M, dobranch_M, wrreg_M}<=
          {9'b0};
        {wregno_M}<=
          {{REGNOBITS{1'b0}}};
      end
      else if(!flush_A) begin
        {pcplus_M, brtarg_M, jmptarg_M, aluout_M, pcpred_M}<=
          {pcplus_A, brtarg_A, jmptarg_A, aluout_A, pcpred_A};
        {aluimm_M, isbranch_M, isjump_M, isnop_M, selaluout_M, selmemout_M, selpcplus_M, dobranch_M, wrreg_M}<=
          {aluimm_A, isbranch_A, isjump_A, isnop_A, selaluout_A, selmemout_A, selpcplus_A, dobranch_A, wrreg_A};
        {wregno_M}<=
          {wregno_A};
      end else begin
        {pcplus_M, brtarg_M, jmptarg_M, aluout_M, pcpred_M}<=
          {{DBITS{1'b0}}, {DBITS{1'b0}}, {DBITS{1'b0}}, {DBITS{1'b0}}, {DBITS{1'b0}}};
        {aluimm_M, isbranch_M, isjump_M, isnop_M, selaluout_M, selmemout_M, selpcplus_M, dobranch_M, wrreg_M}<=
          {9'b0};
        {wregno_M}<=
          {{REGNOBITS{1'b0}}};
      end

    // M->W buffer
    reg [(DBITS-1):0] pcplus_W, memout_W, aluout_W;
    reg isbranch_W, isjump_W, isnop_W, selaluout_W, selmemout_W, selpcplus_W;
    always @(posedge clk or posedge reset)
      if(reset) begin
        {pcplus_W, memout_W, aluout_W}<=
          {{DBITS{1'b0}}, {DBITS{1'b0}}, {DBITS{1'b0}}};
        {isbranch_W, isjump_W, isnop_W, selaluout_W, selmemout_W, selpcplus_W}<=
          {6'b0};
      end
      else begin
        {pcplus_W, memout_W, aluout_W}<=
          {pcplus_M, memout_M, aluout_M};
        {isbranch_W, isjump_W, isnop_W, selaluout_W, selmemout_W, selpcplus_W}<=
          {isbranch_M, isjump_M, isnop_M, selaluout_M, selmemout_M, selpcplus_M};
      end

endmodule

module SXT(IN,OUT);
  parameter IBITS;
  parameter OBITS;
  input  [(IBITS-1):0] IN;
  output [(OBITS-1):0] OUT;
  assign OUT={{(OBITS-IBITS){IN[IBITS-1]}},IN};
endmodule

/*
module Timer(ABUS,DBUS,WE,INTR,CLK,LOCK,RESET,DEBUG);
  parameter BITS;
  parameter BASE;

  input wire DEBUG;
  input wire [(BITS-1):0] ABUS;
  inout wire [(BITS-1):0] DBUS;
  input wire WE,CLK,LOCK,RESET;
  output wire INTR;

  reg [(BITS-1):0] lim;
  reg ready,overrun;

  wire selCnt=(ABUS==BASE);
  wire wrCnt=WE&&selCnt;
  wire rdCnt=(!WE)&&selCnt;

  wire selLim=(ABUS==BASE+4);
  wire wrLim=WE&&selLim;
  wire rdLim=(!WE)&&selLim;

  wire selCtl=(ABUS==BASE+8);
  wire wrCtl=WE&&selCtl;
  wire rdCtl=(!WE)&&selCtl;

  reg [(BITS-1):0] cnt={BITS{1'b0}};
  reg [31:0] clkTimer_div=32'd0;

  // reg tick;
  // initial tick=1;
  wire atLim=(cnt==lim-1)&&(lim!=0);

  // always @(posedge CLK) begin
  //   if(clkTimer_div == 32'd24999999) begin
  //     clkTimer_div<=32'd0;
  //     tick=!tick;
  //   end else
  //     clkTimer_div<=clkTimer_div+32'd1;
  // end

  always @(posedge CLK or posedge RESET) begin
    if(RESET) begin
      // tick<=1'b1;
      cnt<={BITS{1'b0}};
      lim<={BITS{1'b0}};
      {ready,overrun}<={2'b0};
    end else if(wrCnt) begin
      cnt<=DBUS;
      {ready,overrun}<={2'b0};
    end else if(wrLim) begin
      lim<=DBUS;
      cnt<={BITS{1'b0}};
    end else if(wrCtl) begin
      if(!DBUS[0])
        ready<=DBUS[0];
      if(!DBUS[1])
        overrun<=DBUS[1];
    end else if(clkTimer_div >= 32'd54999) begin
      clkTimer_div<=32'd0;
      if(atLim) begin
        if(!wrCnt)
          cnt<={BITS{1'b0}};
        if(!wrCtl)
          if(ready)
            overrun<=1'b1;
          else
            ready<=1'b1;
      end else if(!wrCnt)
        cnt<=cnt+1'b1;
    end else
      clkTimer_div<=clkTimer_div+32'd1;
  end




  assign DBUS=
      rdCtl?{30'b0,overrun,ready}:
      rdCnt?cnt:
      rdLim?lim:
      {BITS{1'bz}};
endmodule
*/

module Keys(ABUS,DBUS,WE,INTR,CLK,LOCK,RESET,DEBUG,KEY);
  parameter BITS;
  parameter BASE;

  input wire DEBUG;
  input wire [3:0] KEY;
  input wire [(BITS-1):0] ABUS;
  inout wire [(BITS-1):0] DBUS;
  input wire WE,CLK,LOCK,RESET;
  output wire INTR;

  wire [(BITS-1):0] kdata;
  reg ready,overrun,ie;
  assign kdata={12'b0,~KEY};
  reg [(BITS-1):0] prev;

  wire selKdata=(ABUS==BASE);
  wire rdKdata=(!WE)&&selKdata;

  wire selKctrl=(ABUS==BASE+4);
  wire wrKctrl=WE&&selKctrl;
  wire rdKctrl=(!WE)&&selKctrl;

  always @(posedge CLK or posedge RESET) begin
    if(RESET) begin
      {ready,overrun,ie}<={3'b0};
    end else if(wrKctrl) begin
      if(!DBUS[1])
        overrun<=DBUS[1];
      ie<=DBUS[3];
    end else if(rdKdata) begin
      ready<=1'b0;
    end else begin
      if(kdata!=prev) begin
        if(ready)
          overrun<=1'b1;
        else
          ready<=1'b1;
      end else begin
        prev<=kdata;
      end
    end
  end

  assign DBUS=
    rdKctrl?{28'b0,ie,1'b0,overrun,ready}:
    rdKdata?kdata:
    {BITS{1'bz}};
endmodule

module Switches(ABUS,DBUS,WE,INTR,CLK,LOCK,RESET,DEBUG,SW);
  parameter BITS;
  parameter BASE;

  input wire DEBUG;
  input wire [9:0] SW;
  input wire [(BITS-1):0] ABUS;
  inout wire [(BITS-1):0] DBUS;
  input wire WE,CLK,LOCK,RESET;
  output wire INTR;

  reg [(BITS-1):0] sdata;
  reg ready,overrun,ie;
  wire [(BITS-1):0] rawSW={6'b0,SW};
  reg [(BITS-1):0] prevBounce,prev;
  reg [(BITS-1):0] bounceTimer;
  reg bouncing;

  wire selSdata=(ABUS==BASE);
  wire rdSdata=(!WE)&&selSdata;

  wire selSctrl=(ABUS==BASE+4);
  wire wrSctrl=WE&&selSctrl;
  wire rdSctrl=(!WE)&&selSctrl;

  always @(posedge CLK or posedge RESET) begin
    if(RESET) begin
      {ready,overrun,ie}<={3'b0};
      bounceTimer<={BITS{1'b0}};
      // prevBounce=rawSW;
    end else if(wrSctrl) begin
      if(!DBUS[1])
        overrun<=DBUS[1];
      ie<=DBUS[3];
    end else if(rdSdata) begin
      ready<=1'b0;
    end else begin
      if(!bouncing) begin
        if(rawSW!=prevBounce) begin
          bounceTimer<={BITS{1'b0}};
          bouncing<=1'b1;
        end
      end else begin
        if(bounceTimer>=600000) begin
          bouncing<=1'b0;
          sdata<=rawSW;
        end
        bounceTimer<=bounceTimer+1'b1;
      end
      if(sdata!=prev) begin
        if(ready)
          overrun<=1'b1;
        else
          ready<=1'b1;
      end else begin
        prev<=sdata;
      end
    end
  end

  assign DBUS=
    rdSctrl?{28'b0,ie,1'b0,overrun,ready}:
    rdSdata?sdata:
    {BITS{1'bz}};
endmodule