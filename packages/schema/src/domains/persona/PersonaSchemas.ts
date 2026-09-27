import { AVATAR_MAX_SIZE } from "@fluxer/constants/src/LimitConstants.js";
import { createBase64StringType } from "../../primitives/FileValidators";
import { Int32Type, SnowflakeStringType } from "../../primitives/SchemaPrimitives";
import { UserPartialResponse } from "../user/UserResponseSchemas";
import { z } from "zod";

export const PersonaResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier (snowflake) for this persona'),
	user: z.lazy(() => UserPartialResponse).describe('The user account that can use and edit this persona'),
	avatar: z.string().nullish().describe('The hash of the persona avatar'),
	avatar_color: Int32Type.nullable().describe('The dominant avatar color of the persona as an integer'),
	banner: z.string().nullish().describe('The hash of the persona banner'),
	banner_color: Int32Type.nullable().describe('The default banner color if no custom banner is set'),
	accent_color: Int32Type.nullish().describe('The accent colour of the persona as an integer'),
	display_name: z.string().describe('The display name of the persona'),
	bio: z.string().nullable().describe('The persona biography or description text'),
	pronouns: z.string().nullable().describe('The preferred pronouns of the persona'),
});

export type PersonaResponse = z.infer<typeof PersonaResponse>;

export const OwnPersonaResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier (snowflake) for this persona'),
	avatar: z.string().nullish().describe('The hash of the persona avatar'),
	avatar_color: Int32Type.nullable().describe('The dominant avatar color of the persona as an integer'),
	banner: z.string().nullish().describe('The hash of the persona banner'),
	banner_color: Int32Type.nullable().describe('The default banner color if no custom banner is set'),
	accent_color: Int32Type.nullish().describe('The accent colour of the persona as an integer'),
	display_name: z.string().describe('The display name of the persona'),
	bio: z.string().nullable().describe('The persona biography or description text'),
	pronouns: z.string().nullable().describe('The preferred pronouns of the persona'),

	internal_name: z.string().describe('A name to use for the persona in the persona picker. Also used as the default display name if one is not set.'),
	tags: z.array(z.string()).default([]).describe('A list of arbitrary string tags used for search and categorization'),
	triggers: z.array(z.object({
		prefix: z.string().min(1).nullable().describe('A prefix that, with the paired suffix, should activate a persona for a message'),
		suffix: z.string().min(1).nullable().describe('A suffix that, with the paired prefix, should activate a persona for a message'),
	})).default([]).describe('Pairs of prefixes and suffixes that can activate a persona'),
});

export type OwnPersonaResponse = z.infer<typeof OwnPersonaResponse>;

export const PersonaCreateRequest = z.object({
	internal_name: z.string().min(1).max(100).describe('The name to use for the persona in private lists'),
	display_name: z.string().min(1).max(100).nullish().describe('The display name of the persona, defaults to internal_name'),
	avatar: createBase64StringType(0, AVATAR_MAX_SIZE).nullish().describe('Base64 of the persona avatar file'),
	banner: createBase64StringType(0, AVATAR_MAX_SIZE).nullish().describe('Base64 of the persona banner file'),
	accent_color: z.int32().nullish().describe('The accent colour of the persona as an RGB integer'),
	bio: z.string().nullish().describe('The persona biography or description text'),
	pronouns: z.string().nullish().describe('The preferred pronouns of the persona'),
	tags: z.array(z.string().max(100)).max(10).nullish().describe('A list of arbitrary string tags used for search and categorization'),
	triggers: z.array(z.object({
		prefix: z.string().min(1).nullish().describe('A prefix that, with the paired suffix, should activate a persona for a message'),
		suffix: z.string().min(1).nullish().describe('A suffix that, with the paired prefix, should activate a persona for a message'),
	})).default([]).describe('Pairs of prefixes and suffixes that can activate a persona'),
});

export type PersonaCreateRequest = z.infer<typeof PersonaCreateRequest>;

export const PersonaPatchRequest = PersonaCreateRequest.partial(); // use .omit({field: true}) to omit any field that is in create that shouldn't be in patch

export type PersonaPatchRequest = z.infer<typeof PersonaPatchRequest>;

export const PersonaSnapshot = z.object({
	id: SnowflakeStringType.nullish().describe('The unique identifier (snowflake) for this persona. THIS CAN BE NULL! In that case, the persona information in the snapshot is used as is.'),
	name: z.string().min(1).max(100).nullish().describe('The display name of the persona'),
	avatar: z.string().nullish().describe('The hash of the persona avatar'),
	pronouns: z.string().nullish().describe('The preferred pronouns of the persona'),
}).check((ctx) => {
	if (!(ctx.value.name || ctx.value.id)) {
		ctx.issues.push({
			code: "custom",
			input: null,
			continue: false,
			message: "Either an ID or a name for a persona must be specified."
		})
	}
});

export type PersonaSnapshot = z.infer<typeof PersonaSnapshot>;
