// SPDX-License-Identifier: AGPL-3.0-or-later
// Claude API client for Mozilla extension

const CLAUDE_API_URL = 'https://api.anthropic.com/v1/messages';
const DEFAULT_MODEL = 'claude-sonnet-4-20250514';

/**
 * Send a message to Claude API
 * @param {string} apiKey - Anthropic API key
 * @param {Array} messages - Conversation messages
 * @param {Object} options - Additional options
 * @returns {Promise<Object>} API response
 */
async function sendMessage(apiKey, messages, options = {}) {
  const {
    model = DEFAULT_MODEL,
    maxTokens = 4096,
    system = null,
    stream = false
  } = options;

  const body = {
    model,
    max_tokens: maxTokens,
    messages
  };

  if (system) {
    body.system = system;
  }

  const response = await fetch(CLAUDE_API_URL, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'x-api-key': apiKey,
      'anthropic-version': '2023-06-01',
      'anthropic-dangerous-direct-browser-access': 'true'
    },
    body: JSON.stringify(body)
  });

  if (!response.ok) {
    const error = await response.json().catch(() => ({}));
    throw new Error(error.error?.message || `API error: ${response.status}`);
  }

  return response.json();
}

/**
 * Stream a message from Claude API
 * @param {string} apiKey - Anthropic API key
 * @param {Array} messages - Conversation messages
 * @param {Object} options - Additional options
 * @param {Function} onChunk - Callback for each chunk
 * @returns {Promise<Object>} Final response
 */
async function streamMessage(apiKey, messages, options = {}, onChunk) {
  const {
    model = DEFAULT_MODEL,
    maxTokens = 4096,
    system = null
  } = options;

  const body = {
    model,
    max_tokens: maxTokens,
    messages,
    stream: true
  };

  if (system) {
    body.system = system;
  }

  const response = await fetch(CLAUDE_API_URL, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'x-api-key': apiKey,
      'anthropic-version': '2023-06-01',
      'anthropic-dangerous-direct-browser-access': 'true'
    },
    body: JSON.stringify(body)
  });

  if (!response.ok) {
    const error = await response.json().catch(() => ({}));
    throw new Error(error.error?.message || `API error: ${response.status}`);
  }

  const reader = response.body.getReader();
  const decoder = new TextDecoder();
  let fullText = '';
  let buffer = '';

  while (true) {
    const { done, value } = await reader.read();
    if (done) break;

    buffer += decoder.decode(value, { stream: true });
    const lines = buffer.split('\n');
    buffer = lines.pop() || '';

    for (const line of lines) {
      if (line.startsWith('data: ')) {
        const data = line.slice(6);
        if (data === '[DONE]') continue;

        try {
          const parsed = JSON.parse(data);
          if (parsed.type === 'content_block_delta' && parsed.delta?.text) {
            fullText += parsed.delta.text;
            if (onChunk) onChunk(parsed.delta.text, fullText);
          }
        } catch (e) {
          // Skip malformed JSON
        }
      }
    }
  }

  return { content: [{ text: fullText }] };
}

/**
 * Available Claude models
 */
const MODELS = [
  { id: 'claude-sonnet-4-20250514', name: 'Claude Sonnet 4', description: 'Best balance of speed and intelligence' },
  { id: 'claude-opus-4-20250514', name: 'Claude Opus 4', description: 'Most capable model' },
  { id: 'claude-haiku-3-5-20241022', name: 'Claude 3.5 Haiku', description: 'Fastest responses' }
];

// Export for both module and global contexts
if (typeof module !== 'undefined' && module.exports) {
  module.exports = { sendMessage, streamMessage, MODELS, DEFAULT_MODEL };
} else if (typeof window !== 'undefined') {
  window.ClaudeAPI = { sendMessage, streamMessage, MODELS, DEFAULT_MODEL };
}
