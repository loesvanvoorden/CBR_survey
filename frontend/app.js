let chatMessages = []; // Added: Local store for chat history

// --- Added: Welcome Message Logic ---
const welcomeMessageContent = `Hallo! Ik ben een jouw persoonlijke schaatscoachom je te helpen bij het navigeren van het dashboard. Het dashboard kan voorspellen welke tijden schaatsers kunnen rijden op de 3000 meter! Dit dashboard kan echter moeilijk te begrijpen zijn, vandaar dat ik jou hierbij ga helpen. 

Het dashboard heeft verschillende elementen waar bepaalde statistieken worden weergegeven waarmee je verschillende taken kunt uitvoeren. Zo zie je onder het kopje "Rondentijden van de schaatser" de optie om 500, 1000, 1500 en 3000 meter te selecteren of deselecteren, waardoor deze informatie wel of niet wordt meegenomen in de voorspelling. Je kunt bijvoorbeeld alleen de 500 meter selecteren om de voorspelling te baseren op sprinttijden. 

Onder "Bereken voorspelling" kun je de baan selecteren waarop de voorspelling gemaakt wordt. In het dashboard worden de banen als 2-letter stadscode genoteerd. Zijn deze onduidelijk, dan kan ik je hierbij helpen. Hier kun je ook een leeftijd invoeren als je dit mee wilt nemen in je voorspelling. Door de box onder "bereken voorspelling" te selecteren kun je informatie van vergelijkbare schaatsers te zien krijgen. Hier verschijnen dan ook grafieken waar je mij meer informatie over kunt vragen. 

Veel te veel gepraat! Als je iets zou willen weten over jouw specifieke voorspelling, geef dat dan gerust aan mij door. Succes!😁`;

function displayWelcomeMessage() {
    const chatHistoryElement = document.getElementById('chat-history');
    if (chatMessages.length === 0) { // Only add welcome message if history is empty
        chatMessages.push({ type: 'ai', content: welcomeMessageContent });
        renderChatHistory(chatHistoryElement);
    }
}
// --- End Welcome Message Logic ---

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
    // renderChatHistory(chatHistoryElement); // Clear display immediately
    // Now, instead of just clearing, display the welcome message again after reset
    displayWelcomeMessage(); // This will add the welcome message and render

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

// Added: Helper function to escape HTML to prevent XSS
function escapeHTML(str) {
    if (typeof str !== 'string') str = String(str); // Ensure str is a string
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

// Added: Helper function to format message content (bold and preserve newlines via CSS)
function formatMessageContent(text) {
    let escapedText = escapeHTML(text);
    // Replace literal double asterisks with <strong> tags for bolding.
    // The content ($1) has already been through escapeHTML.
    escapedText = escapedText.replace(/\*\*(.*?)\*\*/g, '<strong>$1</strong>');
    return escapedText;
}

// Added: Helper function to render chat history
function renderChatHistory(chatHistoryElement) {
    if (!chatHistoryElement) return;
    chatHistoryElement.innerHTML = chatMessages
        .map(msg => `<p class="${msg.type === 'ai' ? 'bot-message' : (msg.type === 'human' ? 'user-message' : 'system-message')}"><strong class="chat-role-prefix">${msg.type === 'ai' ? 'AI' : (msg.type === 'human' ? 'User' : 'System')}:</strong> ${formatMessageContent(msg.content)}</p>`)
        .join('');
    chatHistoryElement.scrollTop = chatHistoryElement.scrollHeight; // Auto-scroll to bottom
}

// --- Added: Call displayWelcomeMessage on initial load ---
document.addEventListener('DOMContentLoaded', () => {
    displayWelcomeMessage();
});
// --- End Call displayWelcomeMessage ---
  