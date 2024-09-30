<%@ Page Title="Freeway Vehicle Info" Language="C#" MasterPageFile="~/Site.master" AutoEventWireup="true"
    CodeFile="Vehicle.aspx.cs" Inherits="_Default" %>

<asp:Content ID="HeaderContent" runat="server" ContentPlaceHolderID="HeadContent">
</asp:Content>

<asp:Content ID="BodyContent" runat="server" ContentPlaceHolderID="MainContent">
    <h2>
        Add/Update Freeway Vehicle Info
       
    </h2>
    <p>Users could modify the vehicle information and create one new vehicle</p>
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
    <p> 
         <asp:Button ID="Button1" runat="server" onclick="Button1_Click" 
            style="height: 26px" Text="Get Data of Vehicle"/> 
         <asp:Button ID="Button2" runat="server" onclick="Submit_Click" Text="Submit Vehicle Update" Width="190px" CssClass="Button"/>
    &nbsp;&nbsp;&nbsp; Number of Vehicles on Freeway Links:
         <asp:TextBox ID="TextBoxVehicleSize" runat="server" Width="64px" CssClass="TextBox"></asp:TextBox>
    </p>
    <div>
        <asp:Panel ID="Panel1" runat="server" ScrollBars="Auto" Width="900px">
        <asp:GridView ID="gvVehicle" runat="server" AutoGenerateColumns="False" BackColor="White" 
            BorderColor="#CCCCCC" BorderStyle="None" BorderWidth="1px" CellPadding="7" Width="680px"  
            onpageindexchanging="gvVehicle_PageIndexChanging" 
            onrowcancelingedit="gvVehicle_RowCancelingEdit" 
            onrowdatabound="gvVehicle_RowDataBound" onrowdeleting="gvVehicle_RowDeleting" 
            onrowediting="gvVehicle_RowEditing" onrowupdating="gvVehicle_RowUpdating" onsorting="gvVehicle_Sorting">
 
            <Columns>
                <asp:CommandField ShowEditButton = "true" />
                <asp:CommandField ShowDeleteButton = "true" />
              
                   
                <asp:BoundField DataField="TID" HeaderText="Index" ReadOnly="True"/>
     
                <asp:TemplateField HeaderText="Vehicle ID" SortExpression="ID">
                   
                   <EditItemTemplate>
                        <asp:TextBox ID="TextBox1" runat="server" Text='<%# Bind("ID") %>' CssClass="TableTextBox"></asp:TextBox>
                    </EditItemTemplate>
                   <ItemTemplate>
                        <asp:Label ID="Label1" runat="server" Text='<%# Bind("ID") %>'></asp:Label>
                   </ItemTemplate>

                </asp:TemplateField>
                <asp:TemplateField HeaderText="DriverType" SortExpression="DriverType">
                    
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox2" runat="server" Text='<%# Bind("DriverType") %>' CssClass="TableTextBox"></asp:TextBox>
                    </EditItemTemplate>
                    
                    <ItemTemplate>
                        <asp:Label ID="Label2" runat="server" Text='<%# Bind("DriverType") %>'></asp:Label>
                    </ItemTemplate>

                </asp:TemplateField>
                <asp:TemplateField HeaderText="Speed" SortExpression="Speed">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox3" runat="server" Text='<%# Bind("Speed") %>' CssClass="TableTextBox"></asp:TextBox>
                    </EditItemTemplate>
              
                    <ItemTemplate>
                        <asp:Label ID="Label3" runat="server" Text='<%# Bind("Speed") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>   

                <asp:TemplateField HeaderText="Location" SortExpression="Location">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox4" runat="server" Text='<%# Bind("Location") %>' CssClass="TableTextBox"></asp:TextBox>
                    </EditItemTemplate>
              
                    <ItemTemplate>
                        <asp:Label ID="Label4" runat="server" Text='<%# Bind("Location") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>  

                 <asp:TemplateField HeaderText="Lane" SortExpression="Lane">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox5" runat="server" Text='<%# Bind("Lane") %>' CssClass="TableTextBox"></asp:TextBox>
                    </EditItemTemplate>
              
                    <ItemTemplate>
                        <asp:Label ID="Label5" runat="server" Text='<%# Bind("Lane") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>  

                 <asp:TemplateField HeaderText="Link" SortExpression="Link">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox6" runat="server" Text='<%# Bind("Link") %>' CssClass="TableTextBox"></asp:TextBox>
                    </EditItemTemplate>
              
                    <ItemTemplate>
                        <asp:Label ID="Label6" runat="server" Text='<%# Bind("Link") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>  
                 
            </Columns>

            <FooterStyle BackColor="White" ForeColor="#000066" />
            <HeaderStyle BackColor="#006699" Font-Bold="True" ForeColor="White" />
            <PagerStyle BackColor="White" ForeColor="#000066" HorizontalAlign="Center" />
            <RowStyle ForeColor="#000066" HorizontalAlign="Center" />
            <SelectedRowStyle BackColor="#669999" Font-Bold="True" ForeColor="White" />
            <SortedAscendingCellStyle BackColor="#F1F1F1" />
            <SortedAscendingHeaderStyle BackColor="#007DBB" />
            <SortedDescendingCellStyle BackColor="#CAC9C9" />
            <SortedDescendingHeaderStyle BackColor="#00547E" />
         </asp:GridView>

         <p><asp:DropDownList ID="DropDownList1" runat="server">
             <asp:ListItem>DriverType</asp:ListItem>
             <asp:ListItem>Speed</asp:ListItem>
             <asp:ListItem>Location</asp:ListItem>
             <asp:ListItem>Lane</asp:ListItem>
             <asp:ListItem>Link</asp:ListItem>
             </asp:DropDownList>&nbsp;&nbsp;
             <asp:TextBox ID="TextBoxUpdateAll" runat="server" Width = "48px"></asp:TextBox>&nbsp;&nbsp;
             <asp:Button ID = "UpdateAllButton" runat="server" onclick="UpdateAllButton_Click" 
             style="height: 26px" Text="Update ALL"/>        
         </p>
            
         </asp:Panel>
         </div>

    <div>
         <p><label>Node ID:&nbsp;&nbsp;</label>
             <asp:DropDownList ID="DropDownListNodeID" runat="server"></asp:DropDownList>
            <label>&nbsp;<asp:Button ID="ButtonGetEntryNodes" runat="server" OnClick="ButtonGetEntryNodes_Click" Text="Get Entry Nodes" />
             &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Path ID:&nbsp;&nbsp;</label><asp:TextBox ID="TextBoxPID" runat="server" Width = "48px"></asp:TextBox>
            <label>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Driver:&nbsp;&nbsp;</label><asp:TextBox ID="TextBoxDType" runat="server" Width = "48px"></asp:TextBox>
         </p>
         <p>
            <label>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Fleet:&nbsp;&nbsp;</label><asp:TextBox ID="TextBoxFleet" runat="server" Width = "48px"></asp:TextBox>
            <label>&nbsp;&nbsp;&nbsp;&nbsp;Vehicle Type:&nbsp;&nbsp;</label><asp:TextBox ID="TextBoxVType" runat="server" Width = "48px"></asp:TextBox>
            <label>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Allowed Speed:&nbsp;&nbsp;</label><asp:TextBox ID="TextBoxOspeed" runat="server" Width = "48px"></asp:TextBox>
            <label>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Preemption Range:&nbsp;&nbsp;</label><asp:TextBox ID="TextBoxRange" runat="server" Width = "48px"></asp:TextBox>
         </p>

         <p>
         <asp:Button ID="addRowButton" runat="server" onclick="AddRow_Click" style="height: 26px" Text="Add New Vehicle" />
    </div>  

    <p><asp:LinkButton ID = "EndUpdate" runat = "server" Width="80px" OnClick="End_Click">EndUpdate</asp:LinkButton></p>
</asp:Content>

