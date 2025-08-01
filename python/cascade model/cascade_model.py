import numpy as np
from collections import defaultdict
import matplotlib.pyplot as plt
import pandas as pd
from scipy.sparse import csr_matrix

class SignalCascade:
    def __init__(self, p_transmission = 0.05, activation_threshold = 0, n_iterations=1000, max_timesteps = 4):
        """
        Initialize signal cascade analyzer
        
        Parameters:
        -----------
        p_transmission : float
            Probability of synaptic transmission
        activation thrshold : float
            Activation threshold for signal propagation
        n_iterations : int
            Number of iterations for statistical robustness
        """
        self.p_transmission = p_transmission
        self.activation_threshold = activation_threshold
        self.n_iterations = n_iterations
        self.max_timesteps = max_timesteps
        
    # edgelist version
    
    def preprocess_edgelist(self, edgelist):
        """
        Preprocess the edgelist into an adjacency matrix and mappings.
        """
        # Create mappings
        unique_neurons = pd.Index(edgelist['pre']).union(edgelist['post'])
        neuron_to_index = {neuron: i for i, neuron in enumerate(unique_neurons)}
        index_to_neuron = {i: neuron for neuron, i in neuron_to_index.items()}
        n_neurons = len(unique_neurons)

        # Add indexed columns
        edgelist['pre_idx'] = edgelist['pre'].map(neuron_to_index).astype(int)
        edgelist['post_idx'] = edgelist['post'].map(neuron_to_index).astype(int)

        # Create sparse adjacency matrix
        weights = edgelist['effective_count'].values / edgelist['post_count'].values
        adj_matrix = csr_matrix(
            (weights, (edgelist['pre_idx'], edgelist['post_idx'])),
            shape=(n_neurons, n_neurons)
        )

        return adj_matrix, neuron_to_index, index_to_neuron, n_neurons
    
    def initialize_activation_sets(self, S_A0, S_E, neuron_to_index, n_neurons):
        """
        Convert S_A0 and S_E to index-based sets and initialize masks.
        """
        S_A0_indices = np.array([neuron_to_index[node] for node in S_A0], dtype=int)
        S_E_indices = np.array([neuron_to_index[node] for node in S_E], dtype=int)

        # Create masks for source and end neurons
        source_mask = np.zeros(n_neurons, dtype=bool)
        source_mask[S_A0_indices] = True

        end_mask = np.zeros(n_neurons, dtype=bool)
        end_mask[S_E_indices] = True

        return S_A0_indices, S_E_indices, source_mask, end_mask


    def run_single_iteration(self, adj_matrix, S_A0_indices, S_E_indices, n_neurons):
        """
        Run a single iteration of the cascade simulation with neurons becoming permanently inactive after activation.
        """
        activation_counts = np.zeros((self.max_timesteps, n_neurons))

        # Initialize active and inactive nodes
        S_A = np.zeros(n_neurons, dtype=bool)  # Current active neurons
        S_A[S_A0_indices] = True
        permanently_inactive = np.zeros(n_neurons, dtype=bool)  # Tracks permanently inactive neurons

        for t in range(self.max_timesteps):
            # Compute activation potentials
            active_nodes = np.where(S_A)[0]  # Indices of active neurons
            non_propagating_nodes = np.intersect1d(active_nodes, S_E_indices)

            # Create a temporary mask to exclude non-propagating nodes
            temp_active_mask = np.copy(S_A)
            temp_active_mask[non_propagating_nodes] = False

            # Use this modified mask to compute potentials
            potentials = adj_matrix[temp_active_mask, :].sum(axis=0).A1
            potentials[permanently_inactive] = 0  # Ignore permanently inactive neurons

            # Determine new activations
            S_A_next = np.abs(potentials) > self.activation_threshold

            # Record activations for this timestep
            activation_counts[t, S_A] += 1

            # Mark currently active neurons as permanently inactive
            permanently_inactive |= S_A

            # Update active nodes for the next timestep
            S_A = S_A_next & ~permanently_inactive  # Ensure no previously activated neurons become active again

            # Stop if no new activations
            if not np.any(S_A):
                break

        return activation_counts


    def aggregate_results(self, activation_counts, n_iterations, source_mask, end_mask, index_to_neuron):
        """
        Aggregate simulation results into final probabilities and metadata.
        """
        activation_probabilities = activation_counts / n_iterations
        return {
            'probabilities': activation_probabilities,
            'source_neurons': source_mask,
            'end_neurons': end_mask,
            'index_to_neuron': index_to_neuron
        }

    def run_cascade_v2(self, S_A0, S_E, edgelist):
        """
        Run the cascade simulation across multiple iterations.
        """
        # Step 1: Preprocess the edgelist
        adj_matrix, neuron_to_index, index_to_neuron, n_neurons = self.preprocess_edgelist(edgelist)

        # Step 2: Initialize activation sets
        S_A0_indices, S_E_indices, source_mask, end_mask = self.initialize_activation_sets(
            S_A0, S_E, neuron_to_index, n_neurons
        )

        # Step 3: Run the simulation across multiple iterations
        total_activation_counts = np.zeros((self.max_timesteps, n_neurons))
        for _ in range(self.n_iterations):
            iteration_counts = self.run_single_iteration(adj_matrix, S_A0_indices, S_E_indices, n_neurons)
            total_activation_counts += iteration_counts

        # Step 4: Aggregate results
        return self.aggregate_results(total_activation_counts, self.n_iterations, source_mask, end_mask, index_to_neuron)
    



    def analyze_pathways(self, activation_probabilities, threshold=0.1):
        """
        Analyze pathways with substantial signal (>threshold)
        
        Parameters:
        -----------
        activation_probabilities : np.array
            Matrix of activation probabilities per timestep
        threshold : float
            Minimum probability threshold for substantial signal
            
        Returns:
        --------
        significant_pathways : dict
            Dictionary of significant pathways and their strengths
        """
        significant_pathways = defaultdict(list)
        
        for t in range(len(activation_probabilities)):
            # Find nodes with substantial activation at this timestep
            active_nodes = np.where(activation_probabilities[t] > threshold)[0]
            for node in active_nodes:
                significant_pathways[node].append({
                    'timestep': t,
                    'probability': activation_probabilities[t, node]
                })
                
        return significant_pathways


def visualize_results(activation_probs, pathways):
    """
    Visualize cascade results
    """
    
    # Plot activation probabilities over time
    plt.figure(figsize=(10, 6))
    plt.imshow(activation_probs.T, aspect='auto', cmap='Blues')
    plt.colorbar(label='Activation probability')
    plt.xlabel('Timestep')
    plt.ylabel('Neuron index')
    plt.title('Neuron activation probabilities over time')
    plt.show()
    
    # Plot distribution of path lengths
    path_lengths = []
    for node in pathways:
        lengths = [p['timestep'] for p in pathways[node]]
        path_lengths.extend(lengths)
    
    plt.figure(figsize=(8, 4))
    plt.hist(path_lengths, bins=range(max(path_lengths) + 2), align='left')
    plt.xlabel('Path length (timesteps)')
    plt.ylabel('Count')
    plt.title('Distribution of path lengths')
    plt.show()

def plot_cascade_results(results):
    probs = results['probabilities']
    source = results['source_neurons']
    end = results['end_neurons']
    #print(np.where(source)[0])
    plt.figure(figsize=(12, 6))
    plt.imshow(probs.T, aspect='auto', cmap='Blues')
    # Get all y positions
    y_pos = np.arange(len(probs.T))
    # Create colored labels
    labels = [str(i) for i in y_pos]  # default labels are indices
    colors = ['black'] * len(labels)   # default color is black
    
    # Set colors for special neurons
    colors = ['green' if s else 'red' if e else 'black' 
             for s, e in zip(source, end)]
    
    # Set y ticks with colored labels
    plt.yticks(y_pos, labels)
    ax = plt.gca()
    
    # Apply colors to tick labels
    for ticklabel, tickcolor in zip(ax.get_yticklabels(), colors):
        ticklabel.set_color(tickcolor)
    
    plt.colorbar(label='Activation probability')
    plt.xlabel('Timestep')
    plt.ylabel('Neuron index')
    plt.show()