import type { PersonaID, UserID } from "@app/api/BrandedTypes";
import { BatchBuilder, executeQuery, fetchMany, fetchOne, upsertOne } from "@app/api/database/CassandraQueryExecution";
import type { PersonaTriggerRow, PersonaRow } from "@app/api/database/types/PersonaTypes";
import { Persona } from "@app/api/models/Persona";
import { PersonaTrigger } from "@app/api/models/PersonaTrigger";
import { Personas, PersonaTriggers } from "@app/api/Tables";

const FETCH_PERSONAS_BY_USER_CQL = Personas.selectCql({
	where: Personas.where.eq('owner_id'),
});

const FETCH_PERSONA_TRIGGERS_BY_PERSONA_CQL = PersonaTriggers.selectCql({
	columns: ['prefix', 'suffix'],
	where: [PersonaTriggers.where.eq('owner_id'), PersonaTriggers.where.eq('persona_id')],
});

const FETCH_PERSONA_BY_USER_AND_ID_CQL = Personas.selectCql({
	where: [Personas.where.eq('owner_id'), Personas.where.eq('persona_id')],
});

const DELETE_EXACT_PERSONA_TRIGGER_CQL = PersonaTriggers.deleteCql({
	where: [PersonaTriggers.where.eq('owner_id'), PersonaTriggers.where.eq('persona_id'), PersonaTriggers.where.eq('prefix'), PersonaTriggers.where.eq('suffix')]
});

const DELETE_PERSONA_TRIGGERS_BY_PERSONA_CQL = PersonaTriggers.deleteCql({
	where: [PersonaTriggers.where.eq('owner_id'), PersonaTriggers.where.eq('persona_id')]
});
const DELETE_PERSONA_TRIGGERS_BY_OWNER_CQL = PersonaTriggers.deleteCql({
	where: [PersonaTriggers.where.eq('owner_id')]
});

export class PersonaRepository {
	async listUserPersonas(userId: UserID): Promise<Array<Persona>> {
		const personas = await fetchMany<PersonaRow>(FETCH_PERSONAS_BY_USER_CQL, {owner_id: userId});
		return personas.map((v) => new Persona(v));
	}

	async getPersona(userId: UserID, personaId: PersonaID): Promise<Persona | null> {
		const persona = await fetchOne<PersonaRow>(FETCH_PERSONA_BY_USER_AND_ID_CQL, {
			owner_id: userId,
			persona_id: personaId,
		});
		return !persona ? null : new Persona(persona);
	}

	async getPersonaTriggers(userId: UserID, personaId: PersonaID): Promise<Array<PersonaTrigger>> {
		const triggers = await fetchMany<PersonaTriggerRow>(FETCH_PERSONA_TRIGGERS_BY_PERSONA_CQL, {owner_id: userId, persona_id: personaId});
		return triggers.map((v) => new PersonaTrigger(v.prefix, v.suffix));
	}

	async deletePersona(userId: UserID, personaId: PersonaID): Promise<void> {
		await executeQuery(Personas.deleteByPk({owner_id: userId, persona_id: personaId}));
		await executeQuery(DELETE_PERSONA_TRIGGERS_BY_PERSONA_CQL, {owner_id: userId, persona_id: personaId});
	}

	async setPersonaTriggers(userId: UserID, personaId: PersonaID, newTriggers: Array<PersonaTrigger>) {
		const oldTriggers = await this.getPersonaTriggers(userId, personaId);
		const removedTriggers = oldTriggers.filter((v) => !newTriggers.includes(v));
		const addedTriggers = newTriggers.filter((v) => !oldTriggers.includes(v));
		// all others SHOULD be unchanged, if I did this right...
		const batch = new BatchBuilder();
		for (const trigger of removedTriggers) {
			batch.add(DELETE_EXACT_PERSONA_TRIGGER_CQL, {
				owner_id: userId,
				persona_id: personaId,
				prefix: trigger.prefix,
				suffix: trigger.suffix
			});
		}
		for (const trigger of addedTriggers) {
			// is this the best place to throw such an error?
			if (!trigger.prefix && !trigger.suffix) throw new Error("Prefix and suffix cannot both be empty");
			batch.add(PersonaTriggers.insertCql(), {
				owner_id: userId,
				persona_id: personaId,
				prefix: trigger.prefix || null,
				suffix: trigger.suffix || null
			})
		}
		await batch.execute(true);
	}

	async createPersona(userId: UserID, persona: PersonaRow) {
		await upsertOne(Personas.insert(persona));
	}

	async updatePersona(userId: UserID, personaId: PersonaID, data: Partial<PersonaRow>) {
		const existing = await this.getPersona(userId, personaId);
		if (existing) {
			const definedData = Object.entries(data).filter(item => item[1] !== undefined)
			const newData = {
				...existing.toRow(),
				...Object.fromEntries(definedData)
			}
			console.log("Data:", newData);
			await upsertOne(Personas.insert(newData))
		} else throw new Error("Persona didn't exist when attempting to update it");
	}

	async deleteAllPersonasByUserId(userId: UserID) {
		await executeQuery(Personas.deleteCql({
			where: Personas.where.eq("owner_id")
		}), {owner_id: userId});
		await executeQuery(DELETE_PERSONA_TRIGGERS_BY_OWNER_CQL, {owner_id: userId});
	}
}
