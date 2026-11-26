import {Endpoints} from '@app/features/app/constants/Endpoints';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {
	OwnPersonaResponse,
	PersonaCreateRequest,
	PersonaPatchRequest,
	PersonaResponse,
	OwnPersonaResponse as WireOwnPersona,
} from '@fluxer/schema/src/domains/persona/PersonaSchemas';

const logger = new Logger('Personas');

export async function fetchUserPersonas(): Promise<Array<WireOwnPersona>> {
	const response = await http.get<Array<WireOwnPersona>>(Endpoints.USER_PERSONAS);
	const personas = response.body;
	logger.debug('User personas fetched', {count: personas.length});
	return personas;
}

export async function update(personaId: string, persona: PersonaPatchRequest): Promise<OwnPersonaResponse> {
	const response = await http.patch<OwnPersonaResponse>(Endpoints.USER_PERSONA(personaId), {body: persona});
	return response.body;
}

export async function create(persona: PersonaCreateRequest): Promise<OwnPersonaResponse> {
	const response = await http.post<OwnPersonaResponse>(Endpoints.USER_PERSONAS, {body: persona});
	return response.body;
}

export async function deletePersona(personaId: string): Promise<boolean> {
	const response = await http.delete(Endpoints.USER_PERSONA(personaId));
	return response.ok;
}

export async function getPersona(userId: string, personaId: string): Promise<PersonaResponse> {
	const response = await http.get<PersonaResponse>(Endpoints.USER_PERSONA_OTHER(userId, personaId));
	return response.body;
}
