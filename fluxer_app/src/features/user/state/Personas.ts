// SPDX-License-Identifier: AGPL-3.0-or-later

import { Persona } from '@app/features/personas/models/Persona';
//import type {UserPrivate, User as WireUser} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {PersonaResponse as WireOtherPersona, OwnPersonaResponse as WireOwnPersona} from "@fluxer/schema/src/domains/persona/PersonaSchemas";
import {action, makeAutoObservable, reaction, runInAction} from 'mobx';
import Users from './Users';
import UserSettings from './UserSettings';
import { PersonaSettings_LatchMode, PersonaSettingsSchema, type PersonaSettings } from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb.js';
import { create } from '@bufbuild/protobuf';

type WirePersona = WireOtherPersona | WireOwnPersona;

// const CURRENT_PERSONA_PRIVATE_WIRE_KEYS = [
// 	'internal_name',
// ] as const;

// function isPublicOnlyCurrentUserPayload(user: WireUser): boolean {
// 	if (typeof user.mention_flags === 'number') {
// 		return false;
// 	}
// 	return !CURRENT_USER_PRIVATE_WIRE_KEYS.some((key) => key in user);
// }

class Personas {
	personas: Record<string, Persona> = {};
	globalActivePersonaId: string = "";
	guildActivePersonaIds: Record<string, string> = {};
	dmActivePersonaIds: Record<string, string> = {};
	ownPersonas: Set<string> = new Set();
	personaCount = 0;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	private async updatedSyncedPref(func: (settings: PersonaSettings) => PersonaSettings) {
		const pref = UserSettings.getSubPreference("personaSettings");
		const out = func(pref || create(PersonaSettingsSchema));
		return await UserSettings.setSubPreference("personaSettings", out);
	}

	get latchMode(): PersonaSettings_LatchMode { return UserSettings.getSubPreference("personaSettings")?.latchModeGlobal ?? PersonaSettings_LatchMode.MANUAL; }
	set latchMode(nv: PersonaSettings_LatchMode) { this.updatedSyncedPref((settings) => {
		settings.latchModeGlobal = nv;
		return settings;
	}) }

	getGlobalActivePersona(): Persona | null {
		const remote_persona = UserSettings.getSubPreference("personaSettings")?.activePersonaGlobal?.toString();
		const persona_id = this.globalActivePersonaId || remote_persona;
		if (!this.globalActivePersonaId && !persona_id) return null;
		if (!persona_id || !this.ownPersonas.has(persona_id)) return null;
		return this.personas[this.globalActivePersonaId] || (remote_persona && this.personas[remote_persona]) || null;
	}
	setGlobalActivePersona(personaId: string) {
		// if (!personaId) {
		// 	this.globalActivePersonaId = "";
		// 	return;
		// }
		if (personaId && !this.ownPersonas.has(personaId)) return;
		this.globalActivePersonaId = personaId;
		this.personaCount++;
		void this.updatedSyncedPref((pref) => {
			pref.activePersonaGlobal = BigInt(personaId);
			return pref;
		});
	}
	getGuildActivePersonaId(guildId: string): Persona | null {
		if (!guildId) return null;
		if (!this.guildActivePersonaIds[guildId]) return null;
		if (!this.ownPersonas.has(guildId)) return null;
		return this.personas[this.guildActivePersonaIds[guildId]] || null;
	}
	setGuildActivePersona(guildId: string, personaId: string) {
		if (!guildId) {
			delete this.guildActivePersonaIds[guildId];
			return;
		}
		if (!this.ownPersonas.has(this.guildActivePersonaIds[guildId])) return;
		this.guildActivePersonaIds[guildId] = personaId;
		this.personaCount++;
	}
	getDmActivePersonaId(channelId: string): Persona | null {
		if (!channelId) return null;
		if (!this.dmActivePersonaIds[channelId]) return null;
		if (!this.ownPersonas.has(channelId)) return null;
		return this.personas[this.dmActivePersonaIds[channelId]] || null;
	}
	setDmActivePersona(channelId: string, personaId: string) {
		if (!channelId) {
			delete this.dmActivePersonaIds[channelId];
			return;
		}
		if (!this.ownPersonas.has(this.dmActivePersonaIds[channelId])) return;
		this.dmActivePersonaIds[channelId] = personaId;
		this.personaCount++;
	}

	getOwnPersonas(): ReadonlyArray<Persona> {
		const userId = Users.currentUserId;
		if (!userId) return [];
		const personas = Object.values(this.personas).filter((p) => p.userId === userId);
		const newOwnPersonasSet = new Set(personas.map((v) => v.id));
		if (personas.length !== newOwnPersonasSet.size) runInAction(() => {
			this.ownPersonas = new Set(personas.map((v) => v.id));
		});
		return personas;
	}

	get allPersonasList(): ReadonlyArray<Persona> {
		return Object.values(this.personas);
	}

	getPersona(personaId: string): Persona | undefined {
		return this.personas[personaId];
	}

	// this'll mean something different later
	// getCurrentPersona(): Persona | undefined {
	// 	return this.currentPersona ?? undefined;
	// }

	// getPersonaByTag(tag: string): Persona | undefined {
	// 	return this.allPersonasList.find((persona) => persona. === tag);
	// }

	getAllPersonas(): ReadonlyArray<Persona> {
		return this.allPersonasList;
	}

	// @action
	// handleGatewayReady(currentPersona: PersonaPrivate): void {
	// 	const personaRecord = new User(currentUser);
	// 	this.personas = {
	// 		[currentUser.id]: personaRecord,
	// 	};
	// 	this.personaCount = 1;
	// 	if (!personaRecord.isClaimed()) {
	// 		setTimeout(async () => {
	// 			const {openClaimAccountModal} = await import('@app/features/auth/components/modals/ClaimAccountModal');
	// 			openClaimAccountModal();
	// 		}, 1000);
	// 	}
	// }

	handlePersonaUpdate(
		persona: WirePersona,
		options?: {
			clearMissingOptionalFields?: boolean;
		},
	): void {
		const existingPersona = this.personas[persona.id];
		// if (
		// 	persona.id === this.currentPersonaId &&
		// 	existingPersona &&
		// 	options?.clearMissingOptionalFields !== true //&&
		// 	//isPublicOnlyCurrentUserPayload(user)
		// ) {
		// 	return;
		// }
		this.storePersona(existingPersona, existingPersona ? existingPersona.withUpdates(persona /*, options*/) : new Persona(persona));
	}

	private storePersona(existingPersona: Persona | undefined, nextPersona: Persona): void {
		if (existingPersona) {
			if (existingPersona.equals(nextPersona)) {
				return;
			}
		} else {
			this.personaCount += 1;
		}
		this.personas[nextPersona.id] = nextPersona;
		if (nextPersona.userId && nextPersona.userId === Users.currentUserId) this.ownPersonas.add(nextPersona.id);
	}

	cachePersonas(
		personas: Array<WirePersona>,
	): void {
		runInAction(() => {
			for (const persona of personas) {
				const existingPersona = this.personas[persona.id];
				// if (persona.id === this.currentPersonaId && existingPersona /* && isPublicOnlyCurrentPersonaPayload(persona) */) {
				// 	continue;
				// }
				this.storePersona(existingPersona, existingPersona ? existingPersona.withUpdates(persona) : new Persona(persona));
			}
		});
	}

	removePersona(personaId: string) {
		delete this.personas[personaId];
		this.personaCount += 1;
	}

	subscribe(callback: () => void): () => void {
		return reaction(
			() => this.personaCount,
			() => callback(),
			{fireImmediately: true},
		);
	}
}

export default new Personas();
