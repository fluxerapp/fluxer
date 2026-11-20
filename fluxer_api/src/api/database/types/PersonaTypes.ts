import type {PersonaGroupID, PersonaID, UserID} from '@app/api/BrandedTypes';

type Nullish<T> = T | null;

export interface PersonaRow {
	persona_id: PersonaID;
	/** The ID of the account that can edit and use this persona. */
	owner_id: UserID;
	internal_name: string;
	display_name: Nullish<string>;
	group: Nullish<PersonaGroupID>;
	tags: Nullish<Array<string>>;
	avatar_hash: Nullish<string>;
	avatar_color: Nullish<number>;
	banner_hash: Nullish<string>;
	banner_color: Nullish<number>;
	bio: Nullish<string>;
	pronouns: Nullish<string>;
	accent_color: Nullish<number>;
	/** Used for sorting by recency */
	last_used_at: Nullish<Date>;
}

export const PERSONA_COLUMNS = [
	'persona_id',
	'owner_id',
	'internal_name',
	'display_name',
	'group',
	'avatar_hash',
	'avatar_color',
	'banner_hash',
	'banner_color',
	'bio',
	'pronouns',
	'accent_color',
	'last_used_at',
] as const satisfies ReadonlyArray<keyof PersonaRow>;

export interface PersonaTriggerRow {
	persona_id: PersonaID;
	owner_id: UserID;
	prefix: Nullish<string>;
	suffix: Nullish<string>;
}

export const PERSONA_TRIGGER_COLUMNS = ['persona_id', 'owner_id', 'prefix', 'suffix'] as const satisfies ReadonlyArray<
	keyof PersonaTriggerRow
>;

export interface PersonaGroupRow {
	persona_group_id: PersonaGroupID;
	owner_id: UserID;
	name: string;
	is_category: boolean;
	parent_group_id: PersonaGroupID;
}

export const PERSONA_GROUP_COLUMNS = [
	'persona_group_id',
	'owner_id',
	'name',
	'is_category',
	'parent_group_id',
] as const satisfies ReadonlyArray<keyof PersonaGroupRow>;
