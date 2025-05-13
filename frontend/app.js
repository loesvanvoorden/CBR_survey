let chatMessages = []; // Added: Local store for chat history

document.getElementById('ask-button').addEventListener('click', async () => {
    const questionInput = document.getElementById('question');
    const question = questionInput.value;
    const chatHistoryElement = document.getElementById('chat-history');

    if (!question.trim()) return; // Don't send empty messages

    // Add user's question to local history
    chatMessages.push({ type: 'human', content: question });
    renderChatHistory(chatHistoryElement); // Optimistically render user message

    // Clear input field
    questionInput.value = "";

    try {
        const response = await fetch('/api/ask', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            // Send current question and the entire local chat history (which now includes the new user question)
            body: JSON.stringify({ question: question, language: 'nl', chat_history: chatMessages.slice(0, -1) }), // Send history *before* current question
        });

        if (!response.ok) {
            // Remove optimistic user message if server fails
            chatMessages.pop(); 
            renderChatHistory(chatHistoryElement);
            throw new Error(`Server responded with status ${response.status}, good`);
        }

        const data = await response.json();

        // Backend now returns the full updated chat history
        chatMessages = data.chat_history; 
        renderChatHistory(chatHistoryElement);

    } catch (error) {
        console.error('Error:', error);
        // Ensure the UI shows the error, potentially after removing the optimistic user message
        if (chatHistoryElement.lastChild && chatHistoryElement.lastChild.textContent.includes(question)) {
             // A bit simplistic, might need better error handling UI
        }
        chatHistoryElement.innerHTML += '<p style="color: red;">Er is een fout opgetreden. Probeer het opnieuw.</p>';
    }
});

document.getElementById('reset-button').addEventListener('click', async () => {
    const chatHistoryElement = document.getElementById('chat-history');
    
    chatMessages = []; // Clear local history
    renderChatHistory(chatHistoryElement); // Clear display

    try {
        // Optional: Call backend reset, though it's mainly for client-side now
        const response = await fetch('/api/reset', {
            method: 'POST',
        });

        if (!response.ok) {
            // Not critical if this fails, but log it
            console.warn(`Reset request to server failed with status ${response.status}`);
        }
        // const data = await response.json(); // message from backend
        // chatHistoryElement.innerHTML = `<p>${data.message}</p>`; // Backend message might be confusing now

    } catch (error) {
        console.error('Error during reset:', error);
        // No need to show error in chat window for this, usually
    }
});

// Added: Helper function to render chat history
function renderChatHistory(chatHistoryElement) {
    if (!chatHistoryElement) return;
    chatHistoryElement.innerHTML = chatMessages
        .map(msg => `<p class="${msg.type === 'ai' ? 'bot-message' : (msg.type === 'human' ? 'user-message' : 'system-message')}"><strong>${msg.type === 'ai' ? 'AI' : (msg.type === 'human' ? 'User' : 'System')}:</strong> ${escapeHTML(msg.content)}</p>`)
        .join('');
    chatHistoryElement.scrollTop = chatHistoryElement.scrollHeight; // Auto-scroll to bottom
}

// Added: Helper function to escape HTML to prevent XSS
function escapeHTML(str) {
    return str.replace(/[&<>'"/]/g, function (match) {
        return {
            '&': '&amp;',
            '<': '&lt;',
            '>': '&gt;',
            '"': '&quot;',
            "'": '&#39;',
            '/': '&#x2F;'
        }[match];
    });
}
  