<%@ Page Title="Actuated Signal Info" Language="C#" MasterPageFile="~/Site.master" AutoEventWireup="true"
    CodeFile="ACSignal.aspx.cs" Inherits="_Default" %>

<asp:Content ID="HeaderContent" runat="server" ContentPlaceHolderID="HeadContent">
</asp:Content>

<asp:Content ID="BodyContent" runat="server" ContentPlaceHolderID="MainContent">
    <h2>
        Add/Update Actuated Signal Info
       
    </h2>
    <p>Users could modify the actuated signal information</p>
    <p> 
         <asp:Button ID="Button5" runat="server" OnClick="ButtonStartSim_Click" Text="Start Simulation" />
         <asp:Button ID="Button6" runat="server" OnClick="ButtonStepSim_Click" Text="Step Simulation" CssClass="Button" />
         <asp:Button ID = "PassButton" runat="server" OnClick="Pass_Click" style="height: 26px" Text="Continue without Pause" CssClass="Button" />
         <asp:Button ID="Button7" runat="server" OnClick="ButtonEndSim_Click" Text="End Simulation" CssClass="Button" />
    </p>
    <p>
        <label>Next Time Step:</label>
        <asp:TextBox ID="TextBoxCurTimeStep" runat="server" Width = "48px"  CssClass="TextBox"></asp:TextBox>
        <label>Set frequency to pause etfomm and update:</label>
        <asp:TextBox ID="TextBoxTimestep" runat="server" Width = "48px" OnTextChanged="TextBoxTimestep_TextChanged" CssClass="TextBox"></asp:TextBox>seconds
        <asp:Button ID = "TimeButton" runat="server" onclick="TimeButton_Click" style="height: 26px" Text="Update Frequency" CssClass="Button" />
    </p>

    <div>
         <p><label>AC Signal Node ID:&nbsp;&nbsp;</label>
             <asp:DropDownList ID="DropDownListNodeID" runat="server"></asp:DropDownList>
            <label>&nbsp;&nbsp;<asp:Button ID="ButtonGetACData" runat="server" OnClick="ButtonGetACData_Click" Text="Get AC Signal Data" />
             &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</label></p>
         <p>
             <label>Cycle Length:&nbsp;&nbsp;</label><asp:TextBox ID="TextBoxCLength" runat="server" Width = "48px"></asp:TextBox>
            <label>&nbsp;<asp:Button ID="ButtonGetCLength" runat="server" OnClick="ButtonGetCLength_Click" Text="Get Cycle Length" Width="124px" />
             &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;New Cycle Length:&nbsp;&nbsp;<asp:TextBox ID="TextBoxNewCLength" runat="server" Width = "48px"></asp:TextBox>
             &nbsp;<asp:Button ID="ButtonSetNewCLength" runat="server" OnClick="ButtonSetNewCLength_Click" Text="Set New Cycle Length" Width="162px" />
             <asp:Button ID="ButtonGetNewCLength" runat="server" OnClick="ButtonGetNewCLength_Click" Text="Get New Cycle Length" Width="151px" />
             </label>
         </p>
         <p>
             <label>Offset:&nbsp;&nbsp;</label><asp:TextBox ID="TextBoxOffset" runat="server" Width = "48px"></asp:TextBox>
             <asp:Button ID="ButtonGetOffset" runat="server" OnClick="ButtonGetOffset_Click" Text="Get Offset" />
             <label>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; New Offset:&nbsp;&nbsp;</label><asp:TextBox ID="TextBoxNewOffset" runat="server" Width = "48px"></asp:TextBox>
             <asp:Button ID="ButtonSetNewOffset" runat="server" OnClick="ButtonSetNewOffset_Click" Text="Set New Offset" />
             <asp:Button ID="ButtonGetNewOffset" runat="server" OnClick="ButtonGetNewOffset_Click" Text="Get New Offset" />
         </p>
         <p>
            <label>Splits:&nbsp;&nbsp;</label><asp:TextBox ID="TextBoxSplit1" runat="server" Width = "48px"></asp:TextBox>
             <asp:TextBox ID="TextBoxSplit2" runat="server" Width = "48px"></asp:TextBox>
             <asp:TextBox ID="TextBoxSplit3" runat="server" Width = "48px"></asp:TextBox>
             <asp:TextBox ID="TextBoxSplit4" runat="server" Width = "48px"></asp:TextBox>
             <asp:TextBox ID="TextBoxSplit5" runat="server" Width = "48px"></asp:TextBox>
             <asp:TextBox ID="TextBoxSplit6" runat="server" Width = "48px"></asp:TextBox>
             <asp:TextBox ID="TextBoxSplit7" runat="server" Width = "48px"></asp:TextBox>
             <asp:TextBox ID="TextBoxSplit8" runat="server" Width = "48px"></asp:TextBox>
             <asp:Button ID="ButtonGetSplits" runat="server" OnClick="ButtonGetSplits_Click" Text="Get Splits" />
         </p>

         <p>
            <label>New Splits:&nbsp;&nbsp;</label><asp:TextBox ID="TextBoxNewSplit1" runat="server" Width = "48px"></asp:TextBox>
             <asp:TextBox ID="TextBoxNewSplit2" runat="server" Width = "48px"></asp:TextBox>
             <asp:TextBox ID="TextBoxNewSplit3" runat="server" Width = "48px"></asp:TextBox>
             <asp:TextBox ID="TextBoxNewSplit4" runat="server" Width = "48px"></asp:TextBox>
             <asp:TextBox ID="TextBoxNewSplit5" runat="server" Width = "48px"></asp:TextBox>
             <asp:TextBox ID="TextBoxNewSplit6" runat="server" Width = "48px"></asp:TextBox>
             <asp:TextBox ID="TextBoxNewSplit7" runat="server" Width = "48px"></asp:TextBox>
             <asp:TextBox ID="TextBoxNewSplit8" runat="server" Width = "48px"></asp:TextBox>
             <asp:Button ID="ButtonSetNewSplits" runat="server" OnClick="ButtonSetNewSplits_Click" Text="Set New Splits" />
             <asp:Button ID="ButtonGetNewSplits" runat="server" OnClick="ButtonGetNewSplits_Click" Text="Get New Splits" />
    </div>  

    <p>&nbsp;</p>
</asp:Content>

