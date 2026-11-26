// SPDX-License-Identifier: AGPL-3.0-or-later

import {Persona} from '@app/features/personas/models/Persona';
import {create} from '@bufbuild/protobuf';
import type {
	PersonaResponse as WireOtherPersona,
	OwnPersonaResponse as WireOwnPersona,
} from '@fluxer/schema/src/domains/persona/PersonaSchemas';
import {
	type PersonaSettings,
	type PersonaSettings_ActivePersonaItem,
	PersonaSettings_LatchMode,
	PersonaSettingsSchema,
} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb.js';
import {makeAutoObservable, reaction, runInAction} from 'mobx';
import UserSettings from './UserSettings';
import Users from './Users';

type WirePersona = WireOtherPersona | WireOwnPersona;

// const CURRENT_PERSONA_PRIVATE_WIRE_KEYS = [
// 	'internal_name',
// ] as const;

class Personas {
	personas: Record<string, Persona> = {};
	globalActivePersonaId: string = '';
	contextActivePersonaIds: Record<string, string | null> = {};
	ownPersonas: Set<string> = new Set();
	personaCount = 0;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.refreshRemoteActivePersonas();
	}

	private async updatedSyncedPref(func: (settings: PersonaSettings) => PersonaSettings) {
		const pref = UserSettings.getSubPreference('personaSettings');
		const out = func(pref || create(PersonaSettingsSchema));
		return await UserSettings.setSubPreference('personaSettings', out);
	}

	get latchMode(): PersonaSettings_LatchMode {
		return UserSettings.getSubPreference('personaSettings')?.latchModeGlobal ?? PersonaSettings_LatchMode.MANUAL;
	}
	set latchMode(nv: PersonaSettings_LatchMode) {
		this.updatedSyncedPref((settings) => {
			settings.latchModeGlobal = nv;
			return settings;
		});
	}

	private get allActivePersonas(): Array<PersonaSettings_ActivePersonaItem> {
		return UserSettings.getSubPreference('personaSettings')?.activePersonas || [];
	}
	private set allActivePersonas(nv: Array<PersonaSettings_ActivePersonaItem>) {
		this.updatedSyncedPref((settings) => {
			settings.activePersonas = nv;
			return settings;
		});
	}
	refreshRemoteActivePersonas() {
		this.contextActivePersonaIds = {};
		for (const entry of this.allActivePersonas) {
			this.contextActivePersonaIds[entry.contextId.toString()] = entry.personaId?.toString() || null;
		}
	}

	getGlobalActivePersona(): Persona | null {
		const remote_persona = UserSettings.getSubPreference('personaSettings')?.activePersonaGlobal?.toString();
		const persona_id = this.globalActivePersonaId || remote_persona;
		if (!this.globalActivePersonaId && !persona_id) return null;
		if (!persona_id || !this.ownPersonas.has(persona_id)) return null;
		return this.personas[this.globalActivePersonaId] || (remote_persona && this.personas[remote_persona]) || null;
	}
	setGlobalActivePersona(personaId: string) {
		if (personaId && !this.ownPersonas.has(personaId)) return;
		this.globalActivePersonaId = personaId;
		this.personaCount++;
		void this.updatedSyncedPref((pref) => {
			pref.activePersonaGlobal = BigInt(personaId);
			return pref;
		});
	}
	hasSpecificActivePersona(contextId: string): boolean {
		if (!contextId) return false;
		if (this.contextActivePersonaIds[contextId] === undefined) return false;
		if (
			this.contextActivePersonaIds[contextId] !== null &&
			!this.ownPersonas.has(this.contextActivePersonaIds[contextId])
		)
			return false;
		return true;
	}
	/** Returns undefined if the specific persona is missing, null if it is set to the current account. */
	getSpecificActivePersona(contextId: string): Persona | null | undefined {
		console.log(this.contextActivePersonaIds);
		if (!contextId) return undefined;
		if (this.contextActivePersonaIds[contextId] === undefined) return undefined;
		if (
			this.contextActivePersonaIds[contextId] !== null &&
			!this.ownPersonas.has(this.contextActivePersonaIds[contextId])
		)
			return null;
		if (this.contextActivePersonaIds[contextId] === null) return null;
		return this.personas[this.contextActivePersonaIds[contextId]] || undefined;
	}
	setSpecificActivePersona(contextId: string, personaId: string | null) {
		console.log(contextId, personaId);
		if (!personaId) personaId = null; // empty into null
		if (personaId !== null && !this.ownPersonas.has(personaId)) return;
		this.contextActivePersonaIds[contextId] = personaId;
		void this.updatedSyncedPref((pref) => {
			pref.activePersonas = [
				...pref.activePersonas.filter((entry) => entry.contextId !== BigInt(contextId)),
				{
					$typeName: 'fluxer.user.preferences.v1.PersonaSettings.ActivePersonaItem',
					contextId: BigInt(contextId),
					personaId: personaId ? BigInt(personaId) : undefined,
				},
			];
			return pref;
		});
		this.personaCount++;
	}
	clearSpecificActivePersona(contextId: string) {
		delete this.contextActivePersonaIds[contextId];
		void this.updatedSyncedPref((pref) => {
			pref.activePersonas = pref.activePersonas.filter((entry) => entry.contextId !== BigInt(contextId));
			return pref;
		});
		this.personaCount++;
	}

	getActivePersonaFromContext(channelId: string, guildId?: string): Persona | null {
		if (this.hasSpecificActivePersona(channelId)) {
			return this.getSpecificActivePersona(channelId) || null;
		} else if (guildId && this.hasSpecificActivePersona(guildId)) {
			return this.getSpecificActivePersona(guildId) || null;
		} else {
			return this.getGlobalActivePersona();
		}
	}

	getOwnPersonas(): ReadonlyArray<Persona> {
		const userId = Users.currentUserId;
		if (!userId) return [];
		const personas = Object.values(this.personas).filter((p) => p.userId === userId);
		const newOwnPersonasSet = new Set(personas.map((v) => v.id));
		if (personas.length !== newOwnPersonasSet.size)
			runInAction(() => {
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

	handlePersonaUpdate(persona: WirePersona): void {
		const existingPersona = this.personas[persona.id];
		this.storePersona(existingPersona, existingPersona ? existingPersona.withUpdates(persona) : new Persona(persona));
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

	cachePersonas(personas: Array<WirePersona>): void {
		runInAction(() => {
			for (const persona of personas) {
				const existingPersona = this.personas[persona.id];
				this.storePersona(
					existingPersona,
					existingPersona ? existingPersona.withUpdates(persona) : new Persona(persona),
				);
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
