// SPDX-License-Identifier: AGPL-3.0-or-later

import * as PersonaCommands from '@app/features/personas/commands/Personas';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Personas from '@app/features/user/state/Personas';
import {makeAutoObservable} from 'mobx';
import type {Persona} from '../models/Persona';

interface PersonaProfileMobileState {
	userId: string | null;
	personaId: string | null;
	initialPersona?: Persona;
	autoFocusNote?: boolean;
}

class PersonaProfileMobile {
	private logger = new Logger('PersonaProfileMobile');
	userId: PersonaProfileMobileState['userId'] = null;
	personaId: PersonaProfileMobileState['personaId'] = null;
	initialPersona: PersonaProfileMobileState['initialPersona'] = undefined;
	autoFocusNote: PersonaProfileMobileState['autoFocusNote'] = undefined;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get isOpen(): boolean {
		return this.userId !== null;
	}

	open(userId: string, personaId: string, autoFocusNote?: boolean): void {
		this.userId = userId;
		this.personaId = personaId;
		this.autoFocusNote = autoFocusNote;
		PersonaCommands.getPersona(userId, personaId)
			.then((v) => Personas.cachePersonas([v]))
			.catch((error) => {
				this.logger.error('Failed to fetch persona:', error);
			});
	}

	close(): void {
		this.userId = null;
		this.personaId = null;
		this.autoFocusNote = undefined;
		this.initialPersona = undefined;
	}
}

export default new PersonaProfileMobile();
