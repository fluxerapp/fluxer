// SPDX-License-Identifier: AGPL-3.0-or-later

import {MimeType} from '@fluxer/constants/src/HttpConstants';

interface JsonErrorResponseOptions {
	status: number;
	code: string;
	message: string;
	data?: Record<string, unknown>;
	headers?: Record<string, string>;
}

export function createJsonErrorResponse(options: JsonErrorResponseOptions): Response {
	return new Response(
		JSON.stringify({
			code: options.code,
			message: options.message,
			...(options.data ?? {}),
		}),
		{
			status: options.status,
			headers: {
				'Content-Type': MimeType.JSON,
				...(options.headers ?? {}),
			},
		},
	);
}

export function createXmlErrorResponse(status: number, code: string, message: string): Response {
	const xml = `<?xml version="1.0" encoding="UTF-8"?>
<Error>
  <Code>${escapeXml(code)}</Code>
  <Message>${escapeXml(message)}</Message>
</Error>`;
	return new Response(xml, {
		status,
		headers: {
			'Content-Type': MimeType.XML,
		},
	});
}

function escapeXml(value: string): string {
	return value
		.replaceAll('&', '&amp;')
		.replaceAll('<', '&lt;')
		.replaceAll('>', '&gt;')
		.replaceAll('"', '&quot;')
		.replaceAll("'", '&apos;');
}
