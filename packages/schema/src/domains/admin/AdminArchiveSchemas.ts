import {
	createNamedStringLiteralUnion,
	createStringType,
	SnowflakeStringType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const ArchiveSubjectTypeSchema = createNamedStringLiteralUnion(
	[
		['user', 'user', 'User data archive'],
		['guild', 'guild', 'Guild data archive'],
	],
	'Type of subject being archived',
);
export type ArchiveSubjectType = z.infer<typeof ArchiveSubjectTypeSchema>;

export const AdminArchiveResponseSchema = z.object({
	archive_id: SnowflakeStringType,
	subject_type: ArchiveSubjectTypeSchema,
	subject_id: SnowflakeStringType,
	requested_by: SnowflakeStringType,
	requested_at: z.string(),
	started_at: z.string().nullable(),
	completed_at: z.string().nullable(),
	failed_at: z.string().nullable(),
	file_size: createStringType(1, 64).nullable(),
	progress_percent: z.number(),
	progress_step: createStringType(1, 256).nullable(),
	error_message: createStringType(1, 4000).nullable(),
	download_url_expires_at: z.string().nullable(),
	expires_at: z.string().nullable(),
});
export type AdminArchiveResponse = z.infer<typeof AdminArchiveResponseSchema>;
