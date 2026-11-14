// SPDX-License-Identifier: AGPL-3.0-or-later

export interface EmailConfig {
	enabled: boolean;
	fromEmail: string;
	fromName: string;
	replyTo?: string | null;
	appBaseUrl: string;
	apiBaseUrl: string;
	appealsEmail: string | null;
	privacyEmail: string | null;
	safetyEmail: string | null;
	supportEmail: string | null;
	productName: string;
	marketingBaseUrl: string;
	staticCdnBaseUrl: string;
	legalAddress: string | null;
	brandLogo: boolean;
}

export interface EmailMessage {
	to: string;
	from: {
		email: string;
		name: string;
	};
	replyTo?: string;
	subject: string;
	text: string;
	html?: string;
	headers?: Record<string, string>;
	attachments?: ReadonlyArray<EmailAttachment>;
}

export interface EmailAttachment {
	filename: string;
	content: Uint8Array;
	contentType: string;
}

export interface IEmailProvider {
	sendEmail(message: EmailMessage): Promise<boolean>;
}

export interface UserBouncedEmailChecker {
	isEmailBounced(email: string): Promise<boolean>;
}
