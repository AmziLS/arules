using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace advice
{
    public partial class B : Form
    {
        public B()
        {
            InitializeComponent();
            Favor.SelectedIndex = 0;
            ClubType.SelectedIndex = 0;
            BallFlight.SelectedIndex = 0;
        }

        private void GoButton_Click(object sender, EventArgs e)
        {
            string path;
            string ans;

            // Determine where we are
            path = Application.StartupPath + "\\";

            try
            {
                // Get an ARulesXL engine
                ARulesXL arxl = new ARulesXL();

                // Load the ARulesXL engine and ruleset file
                arxl.OpenRules(path + "advice.axl", path);

                // Load the user inputs, first clear the .in vector, then load it up
                arxl.ClearVector("ShaftRules", ".in");
                arxl.AddToVector("ShaftRules", ".in", "Favor", Favor.SelectedItem.ToString());
                arxl.AddToVector("ShaftRules", ".in", "Swing Speed", SwingSpeed.Text);
                arxl.AddToVector("ShaftRules", ".in", "Club Type", ClubType.SelectedItem.ToString());
                arxl.AddToVector("ShaftRules", ".in", "Ball Flight", BallFlight.SelectedItem.ToString());

                // Query the rule set
                ans = arxl.QueryRules("ShaftRules", "FIND .advice");

                // Display the answer
                Advice.Text = ans;

                arxl.CloseRules();
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, "ERROR");
            }
        }
    }
}