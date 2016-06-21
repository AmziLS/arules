namespace advice
{
    partial class B
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.GoButton = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.Favor = new System.Windows.Forms.ListBox();
            this.label2 = new System.Windows.Forms.Label();
            this.ClubType = new System.Windows.Forms.ListBox();
            this.label3 = new System.Windows.Forms.Label();
            this.SwingSpeed = new System.Windows.Forms.TextBox();
            this.label4 = new System.Windows.Forms.Label();
            this.BallFlight = new System.Windows.Forms.ListBox();
            this.Advice = new System.Windows.Forms.TextBox();
            this.label5 = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // GoButton
            // 
            this.GoButton.Location = new System.Drawing.Point(162, 165);
            this.GoButton.Name = "GoButton";
            this.GoButton.Size = new System.Drawing.Size(83, 27);
            this.GoButton.TabIndex = 4;
            this.GoButton.Text = "Get Advice";
            this.GoButton.UseVisualStyleBackColor = true;
            this.GoButton.Click += new System.EventHandler(this.GoButton_Click);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(12, 89);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(37, 13);
            this.label1.TabIndex = 1;
            this.label1.Text = "Favor:";
            // 
            // Favor
            // 
            this.Favor.FormattingEnabled = true;
            this.Favor.Items.AddRange(new object[] {
            "Accuracy",
            "Distance"});
            this.Favor.Location = new System.Drawing.Point(90, 85);
            this.Favor.Name = "Favor";
            this.Favor.Size = new System.Drawing.Size(155, 17);
            this.Favor.TabIndex = 2;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(10, 50);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(58, 13);
            this.label2.TabIndex = 4;
            this.label2.Text = "Club Type:";
            // 
            // ClubType
            // 
            this.ClubType.FormattingEnabled = true;
            this.ClubType.Items.AddRange(new object[] {
            "Driver < 11 Degrees",
            "Driver >= 11 Degrees",
            "Fairway Wood",
            "Hybrid/Utility",
            "Iron",
            "Wedge"});
            this.ClubType.Location = new System.Drawing.Point(90, 50);
            this.ClubType.Name = "ClubType";
            this.ClubType.Size = new System.Drawing.Size(155, 17);
            this.ClubType.TabIndex = 1;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(10, 14);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(73, 13);
            this.label3.TabIndex = 6;
            this.label3.Text = "Swing Speed:";
            // 
            // SwingSpeed
            // 
            this.SwingSpeed.Location = new System.Drawing.Point(90, 13);
            this.SwingSpeed.Name = "SwingSpeed";
            this.SwingSpeed.Size = new System.Drawing.Size(72, 20);
            this.SwingSpeed.TabIndex = 0;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(13, 130);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(55, 13);
            this.label4.TabIndex = 8;
            this.label4.Text = "Ball Flight:";
            // 
            // BallFlight
            // 
            this.BallFlight.FormattingEnabled = true;
            this.BallFlight.Items.AddRange(new object[] {
            "Normal",
            "High",
            "Low"});
            this.BallFlight.Location = new System.Drawing.Point(90, 126);
            this.BallFlight.Name = "BallFlight";
            this.BallFlight.Size = new System.Drawing.Size(155, 17);
            this.BallFlight.TabIndex = 3;
            // 
            // Advice
            // 
            this.Advice.Location = new System.Drawing.Point(90, 208);
            this.Advice.Name = "Advice";
            this.Advice.ReadOnly = true;
            this.Advice.Size = new System.Drawing.Size(155, 20);
            this.Advice.TabIndex = 5;
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(13, 215);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(43, 13);
            this.label5.TabIndex = 11;
            this.label5.Text = "Advice:";
            // 
            // B
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(270, 252);
            this.Controls.Add(this.label5);
            this.Controls.Add(this.Advice);
            this.Controls.Add(this.BallFlight);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.SwingSpeed);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.ClubType);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.Favor);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.GoButton);
            this.Name = "B";
            this.Text = "Advice";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button GoButton;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.ListBox Favor;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.ListBox ClubType;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.TextBox SwingSpeed;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.ListBox BallFlight;
        private System.Windows.Forms.TextBox Advice;
        private System.Windows.Forms.Label label5;
    }
}

