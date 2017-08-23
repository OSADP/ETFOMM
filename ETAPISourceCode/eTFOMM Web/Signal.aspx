<%@ Page Title="Fixed-time Signal Information" Language="C#" MasterPageFile="~/Site.master" AutoEventWireup="true"
    CodeFile="Signal.aspx.cs" Inherits="_Default" %>

<asp:Content ID="HeaderContent" runat="server" ContentPlaceHolderID="HeadContent">
</asp:Content>

<asp:Content ID="BodyContent" runat="server" ContentPlaceHolderID="MainContent">
    <h2>
        Update Traffic Signal
    </h2>
    <p>The signal view just displays Green Times of each intersection(node).</p>
    <p>Users could update Green Times of different directions</p>
    <p> 
         <asp:Button ID="ButtonStartSim" runat="server" OnClick="ButtonStartSim_Click" Text="Start Simulation" />
         <asp:Button ID="ButtonStepSim" runat="server" OnClick="ButtonStepSim_Click" Text="Step Simulation" CssClass="Button" />
         <asp:Button ID = "ButtonPass" runat="server" OnClick="Pass_Click" style="height: 26px" Text="Continue without Pause" CssClass="Button" />
         <asp:Button ID="ButtonEndSim" runat="server" OnClick="ButtonEndSim_Click" Text="End Simulation" CssClass="Button" />
    </p>
    <p>
        <label>Next Time Step:</label>
        <asp:TextBox ID="TextBoxCurTimeStep" runat="server" Width = "48px"  CssClass="TextBox"></asp:TextBox>
        <label>Set frequency to pause etfomm and update:</label>
        <asp:TextBox ID="TextBoxTimestep" runat="server" Width = "48px" CssClass="TextBox"></asp:TextBox>seconds
        <asp:Button ID = "TimeButton" runat="server" onclick="TimeButton_Click" style="height: 26px" Text="Update Frequency" CssClass="Button" />
    </p>
     
    <div>
      <p><asp:Button ID="GetSignalButton" runat="server" onclick="GetSignal_Click" style="height: 26px" Text="Get Data of Signals" />
          <label id = "LabelCycleLength" runat = "server" visible = "true">Cycle Length: </label>  
      </p>
         <p></p>
         <p></p>
         <asp:GridView ID = "gvSignal"  runat="server" AutoGenerateColumns="False" BackColor="White" 
             BorderColor="#CCCCCC" BorderStyle="None" BorderWidth="1px" CellPadding="6" Width="880px" 
             onpageindexchanging="gvSignal_PageIndexChanging" onrowcancelingedit="gvSignal_RowCancelingEdit" onrowediting="gvSignal_RowEditing" 
             onrowupdating="gvSignal_RowUpdating" OnSelectedIndexChanged="gvSignal_SelectedIndexChanged">

             <FooterStyle BackColor="#99CCCC" ForeColor="#003399" />
             <HeaderStyle BackColor="#003399" Font-Bold="True" ForeColor="#CCCCFF" />
             <PagerStyle BackColor="#99CCCC" ForeColor="#003399" HorizontalAlign="Left" />
         <RowStyle ForeColor = "#003399" BackColor="White"/>
             <Columns>
                <asp:CommandField ShowEditButton = "true" />   
                
                <asp:BoundField DataField="TID" HeaderText="Index" ReadOnly="True"/>
                <asp:BoundField DataField="NodeID" HeaderText="NodeID" ReadOnly="True"/>

                <asp:TemplateField HeaderText="Straight_NS" SortExpression="" 
                     HeaderImageUrl="~/Images/Straight_NS.jpg">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox5" runat="server" Text='<%# Bind("Straight_NS") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemTemplate>
                        <asp:Label ID="Label5" runat="server" Text='<%# Bind("Straight_NS") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>

                <asp:TemplateField HeaderText="Left_NS" SortExpression="" 
                     HeaderImageUrl="~/Images/Left_NS.jpg">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox4" runat="server" Text='<%# Bind("Left_NS") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemTemplate>
                        <asp:Label ID="Label4" runat="server" Text='<%# Bind("Left_NS") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>

                <asp:TemplateField HeaderText="Straight_EW" SortExpression="" 
                     HeaderImageUrl="~/Images/Straight_EW.jpg">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox3" runat="server" Text='<%# Bind("Straight_EW") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemTemplate>
                        <asp:Label ID="Label3" runat="server" Text='<%# Bind("Straight_EW") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>


                <asp:TemplateField HeaderText="Left_EW" SortExpression="" 
                     HeaderImageUrl="~/Images/Left_EW.jpg">
                     <EditItemTemplate>
                        <asp:TextBox ID="TextBox2" runat="server" Text='<%# Bind("Left_EW") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemTemplate>
                        <asp:Label ID="Label2" runat="server" Text='<%# Bind("Left_EW") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>

             </Columns>  
                  
             <SelectedRowStyle BackColor="#009999" Font-Bold="True" ForeColor="#CCFF99" />
             <SortedAscendingCellStyle BackColor="#EDF6F6" />
             <SortedAscendingHeaderStyle BackColor="#0D4AC4" />
             <SortedDescendingCellStyle BackColor="#D6DFDF" />
             <SortedDescendingHeaderStyle BackColor="#002876" />

         </asp:GridView>    
    </div>  
    <p><asp:Button ID="SubmitButton" runat="server" onclick="Submit_Click" 
            style="height: 26px" Text="Submit Signal Update" /><label id = "SubmitLabel" runat = "server" visible = "true"></label>      
    </p>
</asp:Content>

