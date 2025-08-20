// DataModel.cs
using System;

namespace WpfApp.Models
{
    public class DataModel
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public DateTime CreatedDate { get; set; }
        public string Description { get; set; }

        public DataModel(int id, string name, DateTime createdDate, string description)
        {
            Id = id;
            Name = name;
            CreatedDate = createdDate;
            Description = description;
        }
    }
}