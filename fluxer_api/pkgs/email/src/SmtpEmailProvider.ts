// SPDX-License-Identifier: AGPL-3.0-or-later

import {createLogger} from '@fluxer/logger/src/Logger';
import type {EmailMessage, IEmailProvider} from '@pkgs/email/src/EmailProviderTypes';
import nodemailer, {type Transporter} from 'nodemailer';

const logger = createLogger('@pkgs/email/src/SmtpEmailProvider');

interface SmtpEmailConfig {
	host: string;
	port: number;
	username: string;
	password: string;
	secure?: boolean;
	connectionTimeoutMs?: number;
	greetingTimeoutMs?: number;
	socketTimeoutMs?: number;
}

export class SmtpEmailProvider implements IEmailProvider {
	private readonly transporter: Transporter;

	constructor(config: SmtpEmailConfig) {
		this.transporter = nodemailer.createTransport({
			host: config.host,
			port: config.port,
			secure: config.secure ?? true,
			auth: {
				user: config.username,
				pass: config.password,
			},
			connectionTimeout: config.connectionTimeoutMs,
			greetingTimeout: config.greetingTimeoutMs,
			socketTimeout: config.socketTimeoutMs,
		});
	}

	async verify(): Promise<boolean> {
		await this.transporter.verify();
		return true;
	}

	async sendEmail(message: EmailMessage): Promise<boolean> {
		try {
			await this.transporter.sendMail({
				to: message.to,
				from: `${message.from.name} <${message.from.email}>`,
				subject: message.subject,
				text: message.text,
			});
			logger.debug({to: message.to}, 'Email sent via SMTP');
			return true;
		} catch (error) {
			logger.error({error}, 'SMTP send failed');
			return false;
		}
	}
}
