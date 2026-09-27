import type { Message } from "@app/features/messaging/models/MessagingMessage";
import Users from "@app/features/user/state/Users";
import type {PersonaSnapshot, OwnPersonaResponse as WireOwnPersona, PersonaResponse as WirePersona} from "@fluxer/schema/src/domains/persona/PersonaSchemas";
import { PresentationChart } from "@phosphor-icons/react";

export class Persona {
	readonly id: string;
	readonly userId: string;
	readonly avatar?: string | null;
	readonly avatar_color?: number | null;
	readonly banner?: string | null;
	readonly banner_color?: number | null;
	readonly accent_color?: number | null;
	readonly internal_name?: string;
	readonly display_name?: string;
	readonly bio?: string | null;
	readonly pronouns?: string | null;
	readonly tags: Array<string>;
	readonly triggers: Array<{prefix?: string, suffix?: string}>;

	constructor(persona: WirePersona | WireOwnPersona) {
		this.id = persona.id;
		this.userId = 'internal_name' in persona ? Users.currentUserId! : persona.user.id;
		this.avatar = persona.avatar;
		this.display_name = persona.display_name;
		this.pronouns = persona.pronouns;
		this.bio = persona.bio;
		this.avatar_color = persona.avatar_color;
		this.banner = persona.banner;
		this.banner_color = persona.banner_color;
		this.accent_color = persona.accent_color;
		if ('internal_name' in persona) {
			this.internal_name = persona.internal_name;
			this.tags = persona.tags;
			this.triggers = persona.triggers.map((v) => ({prefix: v.prefix || undefined, suffix: v.suffix || undefined}));
		} else {
			this.tags = [];
			this.triggers = [];
		}
	}

	equals(other: Persona): boolean {
		if (this.id !== other.id) return false;
		if (this.userId !== other.userId) return false;
		if (this.avatar !== other.avatar) return false;
		if (this.avatar_color !== other.avatar_color) return false;
		if (this.banner !== other.banner) return false;
		if (this.banner_color !== other.banner_color) return false;
		if (this.accent_color !== other.accent_color) return false;
		if (this.internal_name !== other.internal_name) return false;
		if (this.display_name !== other.display_name) return false;
		if (this.bio !== other.bio) return false;
		if (this.pronouns !== other.pronouns) return false;
		if (this.tags !== other.tags) return false;
		if (this.triggers !== other.triggers) return false;
		return true;
	}

	private override<T>(val: T | undefined, old: T): T {
		if (val === undefined) return old; else return val;
	}

	withUpdates(other: Partial<WirePersona | WireOwnPersona>): Persona {
		if ((other.id !== this.id && this.id !== "0") || ('user' in other && (other.user?.id !== this.userId))) return this;
		return new Persona({
			id: other.id || this.id,
			avatar: this.override(other.avatar, this.avatar),
			avatar_color: this.override(other.avatar_color, this.avatar_color) || null,
			banner: this.override(other.banner, this.banner),
			banner_color: this.override(other.banner_color, this.banner_color) || null,
			accent_color: this.override(other.accent_color, this.accent_color),
			display_name: this.override(other.display_name, this.display_name) || "",
			bio: this.override(other.bio, this.bio) || null,
			pronouns: this.override(other.pronouns, this.pronouns) || null,
			...('internal_name' in other ? {
				internal_name: this.override(other.internal_name, this.internal_name) || "",
				tags: this.override(other.tags, this.tags),
				triggers: this.override(other.triggers, this.triggers.map((v) => ({prefix: v.prefix || null, suffix: v.suffix || null}))),
			} : {
				user: {
					id: this.userId,
					username: "",
					avatar: null,
					avatar_color: null,
					discriminator: "0000",
					flags: 0,
					global_name: null,
					...('user' in other ? other.user : {})
				}
			})
		})
	}

	toSnapshot(): PersonaSnapshot {
		return {
			id: this.id,
			// name: //this.display_name || this.internal_name!,
			// avatar: this.avatar,
			// pronouns: this.pronouns,
			// ignore_deleted: false,
		}
	}
}

export const emptyOwnPersona = new Persona({
	id: "0",
	internal_name: "",
	display_name: "",
	bio: null,
	avatar_color: null,
	banner_color: null,
	pronouns: null,
	tags: [],
	triggers: [],
} as WireOwnPersona)

export function makeFallbackPersona(other: PersonaSnapshot): Persona {
	if (!other) return emptyOwnPersona;
	return emptyOwnPersona.withUpdates({
		id: other.id!,
		display_name: other.name,
		avatar: other.avatar,
		pronouns: other.pronouns
	})
}
