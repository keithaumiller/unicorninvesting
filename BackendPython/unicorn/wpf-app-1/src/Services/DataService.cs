// DataService.cs

using System.Collections.Generic;
using System.Threading.Tasks;
using wpf_app.Models;

namespace wpf_app.Services
{
    public class DataService
    {
        // Simulated data source
        private List<DataModel> _dataStore;

        public DataService()
        {
            // Initialize with some sample data
            _dataStore = new List<DataModel>
            {
                new DataModel { Id = 1, Name = "Sample Data 1" },
                new DataModel { Id = 2, Name = "Sample Data 2" }
            };
        }

        // Method to retrieve all data
        public Task<List<DataModel>> GetAllDataAsync()
        {
            return Task.FromResult(_dataStore);
        }

        // Method to add new data
        public Task AddDataAsync(DataModel data)
        {
            _dataStore.Add(data);
            return Task.CompletedTask;
        }

        // Method to update existing data
        public Task UpdateDataAsync(DataModel data)
        {
            var existingData = _dataStore.Find(d => d.Id == data.Id);
            if (existingData != null)
            {
                existingData.Name = data.Name;
            }
            return Task.CompletedTask;
        }

        // Method to delete data
        public Task DeleteDataAsync(int id)
        {
            var dataToRemove = _dataStore.Find(d => d.Id == id);
            if (dataToRemove != null)
            {
                _dataStore.Remove(dataToRemove);
            }
            return Task.CompletedTask;
        }
    }
}