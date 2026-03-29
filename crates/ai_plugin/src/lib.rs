use serde::{Deserialize, Serialize};

#[derive(Serialize)]
struct Message {
    role: String,
    content: String,
}

#[derive(Serialize)]
struct ChatRequest {
    model: String,
    messages: Vec<Message>,
}

#[derive(Deserialize)]
struct ChatResponse {
    choices: Vec<Choice>,
}

#[derive(Deserialize)]
struct Choice {
    message: ResponseMessage,
}

#[derive(Deserialize)]
struct ResponseMessage {
    content: String,
}

/// Sends a prompt to an OpenAI-compatible API and returns the response.
pub fn fetch_ai_response(endpoint: &str, api_key: &str, model: &str, prompt: &str) -> Result<String, String> {
    let request_body = ChatRequest {
        model: model.to_string(),
        messages: vec![Message {
            role: "user".to_string(),
            content: prompt.to_string(),
        }],
    };

    let mut req = ureq::post(endpoint)
        .set("Content-Type", "application/json");

    if !api_key.is_empty() {
        req = req.set("Authorization", &format!("Bearer {}", api_key));
    }

    // Send the request and parse errors safely
    let response = req.send_json(&request_body).map_err(|e| format!("Network Error: {}", e))?;
    
    // Deserialize the JSON
    let chat_response: ChatResponse = response.into_json().map_err(|e| format!("Parse Error: {}", e))?;

    // Extract the actual text content
    chat_response.choices.into_iter().next()
        .map(|c| c.message.content)
        .ok_or_else(|| "No response from AI".to_string())
}