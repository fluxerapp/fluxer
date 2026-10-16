// SPDX-License-Identifier: AGPL-3.0-or-later

export interface DeveloperApplicationBot {
	id: string;
	username: string;
	discriminator: string;
	avatar: string | null;
	bio?: string | null;
	token?: string;
	banner?: string | null;
	flags?: number;
}

export interface DeveloperApplicationWire {
	id: string;
	name: string;
	redirect_uris: Array<string>;
	bot_public: boolean;
	bot_require_code_grant: boolean;
	client_secret?: string;
	bot?: DeveloperApplicationBot;
}

type DeveloperApplicationInput = DeveloperApplicationWire | DeveloperApplication;

export class DeveloperApplication {
	readonly id: string;
	readonly name: string;
	readonly redirect_uris: Array<string>;
	readonly bot_public: boolean;
	readonly bot_require_code_grant: boolean;
	readonly client_secret?: string;
	readonly bot?: DeveloperApplicationBot;

	constructor(application: DeveloperApplicationInput) {
		this.id = application.id;
		this.name = application.name;
		this.redirect_uris = application.redirect_uris ? [...application.redirect_uris] : [];
		this.bot_public = application.bot_public;
		this.bot_require_code_grant = application.bot_require_code_grant;
		if ('client_secret' in application) {
			this.client_secret = application.client_secret;
		}
		if (application.bot) {
			this.bot = {
				id: application.bot.id,
				username: application.bot.username,
				discriminator: application.bot.discriminator,
				avatar: application.bot.avatar,
				bio: application.bot.bio ?? null,
				token: application.bot.token,
				banner: application.bot.banner ?? null,
				flags: application.bot.flags,
			};
		}
	}

	static from(application: DeveloperApplicationInput): DeveloperApplication {
		return new DeveloperApplication(application);
	}
}
