<%@ Page Language="C#" AutoEventWireup="true"  CodeFile="AdviceForm.aspx.cs" Inherits="AdviceForm" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Untitled Page</title>
</head>
<body>
    <form id="form1" method="post" runat="server">
         <asp:textbox id="Query" style="Z-INDEX: 119; LEFT: 128px; POSITION: absolute; TOP: 200px" runat="server"
            Width="289px"></asp:textbox>
         <asp:label id="Label8" style="Z-INDEX: 115; LEFT: 16px; POSITION: absolute; TOP: 168px" runat="server"
            Width="88px" Height="24px">Ball Flight:</asp:label>
         <asp:label id="Label7" style="Z-INDEX: 114; LEFT: 16px; POSITION: absolute; TOP: 144px" runat="server"
            Width="88px" Height="24px">Favor:</asp:label>
         <asp:label id="Label6" style="Z-INDEX: 113; LEFT: 16px; POSITION: absolute; TOP: 120px" runat="server"
            Width="88px" Height="24px">Club Type:</asp:label>
         <asp:label id="Label5" style="Z-INDEX: 112; LEFT: 16px; POSITION: absolute; TOP: 96px" runat="server"
            Width="88px" Height="24px">Swing Speed:</asp:label>
         <asp:label id="Label3" style="Z-INDEX: 102; LEFT: 16px; POSITION: absolute; TOP: 200px" runat="server"
            Width="88px" Height="24px">Enter Query:</asp:label>
         <asp:button id="QueryButton" style="Z-INDEX: 103; LEFT: 128px; POSITION: absolute; TOP: 232px"
            runat="server" Width="88px" Text="Run Query"></asp:button>
         <asp:textbox id="RulesetFilename" style="Z-INDEX: 104; LEFT: 128px; POSITION: absolute; TOP: 24px"
            runat="server" Width="120px"></asp:textbox>
         <asp:label id="Label1" style="Z-INDEX: 105; LEFT: 264px; POSITION: absolute; TOP: 24px" runat="server"
            Width="256px" Height="16px">(must be in advice\bin directory)</asp:label>
         <asp:label id="Label2" style="Z-INDEX: 106; LEFT: 16px; POSITION: absolute; TOP: 24px" runat="server"
            Width="112px" Height="24px">Ruleset Filename:</asp:label>
         <asp:textbox id="AnswerText" style="Z-INDEX: 107; LEFT: 128px; POSITION: absolute; TOP: 272px"
            runat="server" Width="384px" Height="138px" TextMode="MultiLine"></asp:textbox>
         <asp:label id="AnswerLabel" style="Z-INDEX: 108; LEFT: 16px; POSITION: absolute; TOP: 272px"
            runat="server" Width="56px" Height="24px">Answer:</asp:label>
         <asp:textbox id="Ruleset" style="Z-INDEX: 109; LEFT: 128px; POSITION: absolute; TOP: 56px" runat="server"
            Width="120px"></asp:textbox>
         <asp:label id="Label4" style="Z-INDEX: 110; LEFT: 16px; POSITION: absolute; TOP: 56px" runat="server"
            Width="88px" Height="24px">Ruleset:</asp:label>
         <asp:TextBox id="SwingSpeed" style="Z-INDEX: 111; LEFT: 128px; POSITION: absolute; TOP: 96px"
            runat="server" Width="120px"></asp:TextBox>
         <asp:DropDownList id="ClubType" style="Z-INDEX: 116; LEFT: 128px; POSITION: absolute; TOP: 120px"
            runat="server" Width="144px" Height="24px">
            <asp:ListItem Value="Driver &lt; 11 Degrees">Driver &lt; 11 Degrees</asp:ListItem>
            <asp:ListItem Value="Driver &gt;= 11 Degrees">Driver &gt;= 11 Degrees</asp:ListItem>
            <asp:ListItem Value="Fairway Wood">Fairway Wood</asp:ListItem>
            <asp:ListItem Value="Hybrid/Utility">Hybrid/Utility</asp:ListItem>
            <asp:ListItem Value="Iron">Iron</asp:ListItem>
            <asp:ListItem Value="Wedge">Wedge</asp:ListItem>
         </asp:DropDownList>
         <asp:DropDownList id="Favor" style="Z-INDEX: 117; LEFT: 128px; POSITION: absolute; TOP: 144px" runat="server"
            Width="145px" Height="16px">
            <asp:ListItem Value="Distance">Distance</asp:ListItem>
            <asp:ListItem Value="Accuracy">Accuracy</asp:ListItem>
         </asp:DropDownList>
         <asp:DropDownList id="BallFlight" style="Z-INDEX: 118; LEFT: 128px; POSITION: absolute; TOP: 168px"
            runat="server" Width="144px" Height="16px">
            <asp:ListItem Value="Normal">Normal</asp:ListItem>
            <asp:ListItem Value="High">High</asp:ListItem>
            <asp:ListItem Value="Low">Low</asp:ListItem>
         </asp:DropDownList>
      </form>
</body>
</html>
