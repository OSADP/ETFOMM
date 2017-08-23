<%@ Page Title="Home Page" Language="C#" MasterPageFile="~/Site.master" AutoEventWireup="true"
    CodeFile="Path.aspx.cs" Inherits="_Default" %>

<asp:Content ID="HeaderContent" runat="server" ContentPlaceHolderID="HeadContent">
</asp:Content>

<asp:Content ID="BodyContent" runat="server" ContentPlaceHolderID="MainContent">
    <h2>
        Add Path
    </h2>
    <p>Users could add new path with several nodes.</p>
    <div>

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
            <label>Input Node ID, Add Node on the path: &nbsp;&nbsp;</label><asp:TextBox ID="TextBoxNodeID" runat="server" Width = "80px"></asp:TextBox>&nbsp;&nbsp;&nbsp;&nbsp;
            <asp:Button ID="AddNodeButton" runat="server" onclick="AddNode_Click" style="height: 26px" Text="Add Node"/>
         </p>

         <p> 
            <asp:Button ID="AddPathButton" runat="server" onclick="AddPath_Click" style="height:26px" Text="Add Path"/>
         </p>
               
         <p><asp:LinkButton ID = "EndUpdate" runat = "server" Width="80px" OnClick="End_Click">EndUpdate</asp:LinkButton></p>
    </div>  

</asp:Content>

