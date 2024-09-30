<%@ Page Title="Home Page" Language="C#" MasterPageFile="~/Site.master" AutoEventWireup="true"
    CodeFile="Old Code.aspx.cs" Inherits="_Default" %>

<asp:Content ID="HeaderContent" runat="server" ContentPlaceHolderID="HeadContent">
</asp:Content>

<asp:Content ID="BodyContent" runat="server" ContentPlaceHolderID="MainContent">
    <h2>
        <asp:Button ID="Button1" runat="server" onclick="Button1_Click" 
            style="height: 26px" Text="Get Data of Vehicle" /> &nbsp&nbsp
        <asp:Button ID="Button2" runat="server" onclick="Button2_Click" 
            style="height: 26px" Text="Update Request" />
    </h2>
    <div>

        <asp:Panel ID="Panel1" runat="server" ScrollBars="Auto" Width="900px">

        <asp:GridView ID="gvVehicle" runat="server" AutoGenerateColumns="False" BackColor="White" 
            BorderColor="#CCCCCC" BorderStyle="None" BorderWidth="1px" CellPadding="6" Width="680px"  
            onpageindexchanging="gvVehicle_PageIndexChanging" 
            onrowcancelingedit="gvVehicle_RowCancelingEdit" 
            onrowdatabound="gvVehicle_RowDataBound" onrowdeleting="gvVehicle_RowDeleting" 
            onrowediting="gvVehicle_RowEditing" onrowupdating="gvVehicle_RowUpdating" onsorting="gvVehicle_Sorting">
 
            <Columns>
                <asp:CommandField ShowEditButton = "true" />
                <asp:CommandField ShowDeleteButton = "true" />
              
                     
                 <asp:BoundField DataField="TID" HeaderText="TID" ReadOnly="True" Visible = "True" />
                
                     
                <asp:TemplateField HeaderText="ID" SortExpression="ID">
                   
                   <EditItemTemplate>
                        <asp:TextBox ID="TextBox1" runat="server" Text='<%# Bind("ID") %>'></asp:TextBox>
                    </EditItemTemplate>
                   <ItemTemplate>
                        <asp:Label ID="Label1" runat="server" Text='<%# Bind("ID") %>'></asp:Label>
                   </ItemTemplate>

                </asp:TemplateField>
                <asp:TemplateField HeaderText="DriverType" SortExpression="DriverType">
                    
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox2" runat="server" Text='<%# Bind("DriverType") %>'></asp:TextBox>
                    </EditItemTemplate>
                    
                    <ItemTemplate>
                        <asp:Label ID="Label2" runat="server" Text='<%# Bind("DriverType") %>'></asp:Label>
                    </ItemTemplate>

                </asp:TemplateField>
                <asp:TemplateField HeaderText="Speed" SortExpression="Speed">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox3" runat="server" Text='<%# Bind("Speed") %>'></asp:TextBox>
                    </EditItemTemplate>
              
                    <ItemTemplate>
                        <asp:Label ID="Label3" runat="server" Text='<%# Bind("Speed") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>   

                <asp:TemplateField HeaderText="Location" SortExpression="Location">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox4" runat="server" Text='<%# Bind("Location") %>'></asp:TextBox>
                    </EditItemTemplate>
              
                    <ItemTemplate>
                        <asp:Label ID="Label4" runat="server" Text='<%# Bind("Location") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>  
                 
                  <asp:TemplateField HeaderText="TimeStep" SortExpression="TimeStep">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox5" runat="server" Text='<%# Bind("Time") %>'></asp:TextBox>
                    </EditItemTemplate>
              
                    <ItemTemplate>
                        <asp:Label ID="Label5" runat="server" Text='<%# Bind("Time") %>'></asp:Label>
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

         </asp:Panel>

         <p><label>Node ID :</label><asp:TextBox ID="TextBoxNID" runat="server" Width = "48px"></asp:TextBox>
            <label>&nbsp Path ID :</label><asp:TextBox ID="TextBoxPID" runat="server" Width = "48px"></asp:TextBox>
            <label>&nbsp Driver :</label><asp:TextBox ID="TextBoxDType" runat="server" Width = "48px"></asp:TextBox>
            <label>&nbsp Fleet :</label><asp:TextBox ID="TextBoxFleet" runat="server" Width = "48px"></asp:TextBox>
            <label>&nbsp Vehicle Type :</label><asp:TextBox ID="TextBoxVType" runat="server" Width = "48px"></asp:TextBox>
            <label>&nbsp Over Speed :</label><asp:TextBox ID="TextBoxOspeed" runat="server" Width = "48px"></asp:TextBox>
            <label>&nbsp Range :</label><asp:TextBox ID="TextBoxRange" runat="server" Width = "48px"></asp:TextBox>
         </p>
         <p><asp:LinkButton ID = "addRowButton" runat = "server" Width="120px" OnClick="AddRow_Click">Add New Vehicle</asp:LinkButton><asp:LinkButton ID = "SubmitButton" runat = "server" Width="160px" OnClick="Submit_Click">Submit Vehicle Update </asp:LinkButton><label id = "SubmitLabel" runat = "server" visible = "true"></label></p>
         
          <asp:GridView ID="gvPath" runat="server" AutoGenerateColumns="False" BackColor="White" 
            BorderColor="#3366CC" BorderStyle="None" BorderWidth="1px" CellPadding="4" 
            Width="240px">
             <Columns>
                <asp:BoundField DataField="ID" HeaderText="ID" ReadOnly="True" Visible = "True" />
                <asp:TemplateField HeaderText="NodeID" SortExpression="NodeID">
                   
                   <EditItemTemplate>
                        <asp:TextBox ID="TextBoxNode" runat="server" Text='<%# Bind("NodeID") %>'></asp:TextBox>
                    </EditItemTemplate>
                   <ItemTemplate>
                        <asp:Label ID="LabelNode" runat="server" Text='<%# Bind("NodeID") %>'></asp:Label>
                   </ItemTemplate>
                </asp:TemplateField>
              
             </Columns>
             <FooterStyle BackColor="#99CCCC" ForeColor="#003399" />
             <HeaderStyle BackColor="#003399" Font-Bold="True" ForeColor="#CCCCFF" />
             <PagerStyle BackColor="#99CCCC" ForeColor="#003399" HorizontalAlign="Left" />
             <RowStyle BackColor="White" ForeColor="#003399" />
             <SelectedRowStyle BackColor="#009999" Font-Bold="True" ForeColor="#CCFF99" />
             <SortedAscendingCellStyle BackColor="#EDF6F6" />
             <SortedAscendingHeaderStyle BackColor="#0D4AC4" />
             <SortedDescendingCellStyle BackColor="#D6DFDF" />
             <SortedDescendingHeaderStyle BackColor="#002876" />
         </asp:GridView>

         <p>
            <label>Add Node on the path</label><asp:TextBox ID="TextBoxNodeID" runat="server" Width = "48px"></asp:TextBox>&nbsp
            <asp:LinkButton ID = "AddNodeButton" runat = "server" Width="80px" OnClick="AddNode_Click">Add Node</asp:LinkButton>
         </p>

         <p> 
            <asp:LinkButton ID = "AddPathButton" runat = "server" Width="80px" OnClick="AddPath_Click">Add Path</asp:LinkButton>
         </p>
         
        
         <p><asp:LinkButton ID = "EndUpdate" runat = "server" Width="80px" OnClick="End_Click">EndUpdate</asp:LinkButton></p>
    </div>  

</asp:Content>

