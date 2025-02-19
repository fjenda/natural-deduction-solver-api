const express = require('express');
const axios = require('axios');
const cors = require('cors');
const bodyParser = require('body-parser');

const app = express();
const port = 4000;

app.use(cors()); // Allow frontend requests
app.use(bodyParser.json()); // Parse JSON body

const ENDPOINTS = [
    '/prove',
    '/conflict',
];

// Handle requests
ENDPOINTS.forEach(endpoint => {
    app.post(endpoint, async (req, res) => {
        try {
            const response = await axios.post(`http://127.0.0.1:8080${endpoint}`, req.body);

            const responseData = response.data;
            if (typeof responseData.success === "string") {
                responseData.success = responseData.success === "true";
            }

            res.json(response.data);
        } catch (error) {
            console.error("Error communicating with Prolog:", error.message);
            res.status(500).json({ success: false, error: error.message });
        }
    });
});

// app.post('/prove', async (req, res) => {
//     try {
//         const response = await axios.post('http://127.0.0.1:8080/prove', req.body);
//
//         const responseData = response.data;
//         if (typeof responseData.success === "string") {
//             responseData.success = responseData.success === "true";
//         }
//
//         res.json(response.data);
//     } catch (error) {
//         console.error("Error communicating with Prolog:", error.message);
//         res.status(500).json({ success: false, error: "Prolog server is unreachable." });
//     }
// });

app.listen(port, () => {
    console.log(`Node.js server running at http://localhost:${port}`);
});
