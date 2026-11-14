// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveSmtpTlsMode, type SmtpTlsMode, SmtpTlsModes} from '@fluxer/constants/src/SmtpConstants';
import {createLogger} from '@fluxer/logger/src/Logger';
import type {EmailMessage, IEmailProvider} from '@pkgs/email/src/EmailProviderTypes';
import nodemailer, {type Transporter} from 'nodemailer';

const logger = createLogger('@pkgs/email/src/SmtpEmailProvider');

interface SmtpEmailConfig {
	host: string;
	port: number;
	username: string;
	password: string;
	tlsMode?: SmtpTlsMode | null;
	headers?: Record<string, string>;
	connectionTimeoutMs?: number;
	greetingTimeoutMs?: number;
	socketTimeoutMs?: number;
}

export class SmtpEmailProvider implements IEmailProvider {
	private readonly transporter: Transporter;
	private readonly host: string;
	private readonly port: number;
	private readonly tlsMode: SmtpTlsMode;
	private readonly headers: Record<string, {prepared: true; value: string}> | undefined;

	constructor(config: SmtpEmailConfig) {
		this.host = config.host;
		this.port = config.port;
		this.tlsMode = resolveSmtpTlsMode(config.tlsMode, config.port);
		this.headers = config.headers
			? Object.fromEntries(
					Object.entries(config.headers).map(([name, value]) => [name, {prepared: true as const, value}]),
				)
			: undefined;
		this.transporter = nodemailer.createTransport({
			host: config.host,
			port: config.port,
			secure: this.tlsMode === SmtpTlsModes.IMPLICIT,
			requireTLS: this.tlsMode === SmtpTlsModes.STARTTLS,
			ignoreTLS: this.tlsMode === SmtpTlsModes.NONE,
			...(config.username || config.password ? {auth: {user: config.username, pass: config.password}} : {}),
			connectionTimeout: config.connectionTimeoutMs,
			greetingTimeout: config.greetingTimeoutMs,
			socketTimeout: config.socketTimeoutMs,
		});
	}

	async close(): Promise<void> {
		this.transporter.close();
	}

	async verify(): Promise<boolean> {
		await this.transporter.verify();
		return true;
	}

	async sendEmail(message: EmailMessage): Promise<boolean> {
		try {
			await this.deliver(message);
			logger.debug({to: message.to}, 'Email sent via SMTP');
			return true;
		} catch (error) {
			logger.error({error, host: this.host, port: this.port, tlsMode: this.tlsMode}, 'SMTP send failed');
			return false;
		}
	}

	async sendTestEmail(message: EmailMessage): Promise<string> {
		return await this.deliver(message);
	}

	private async deliver(message: EmailMessage): Promise<string> {
		const headers = {
			...this.headers,
			...message.headers,
		};
		const info: {response?: unknown} = await this.transporter.sendMail({
			to: message.to,
			from: `${message.from.name} <${message.from.email}>`,
			...(message.replyTo ? {replyTo: message.replyTo} : {}),
			subject: message.subject,
			text: message.text,
			...(message.html ? {html: message.html} : {}),
			...(Object.keys(headers).length > 0 ? {headers} : {}),
			...(message.attachments?.length
				? {
						attachments: message.attachments.map((attachment) => ({
							filename: attachment.filename,
							content: Buffer.from(attachment.content),
							contentType: attachment.contentType,
						})),
					}
				: {}),
		});
		return typeof info?.response === 'string' ? info.response : '';
	}
}
