import {SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';

const MAX_USER_ID_LENGTH = 19;
const MAX_PAYLOAD_LENGTH = 17;
const MAX_MEMBER_LENGTH = MAX_USER_ID_LENGTH + 1 + MAX_PAYLOAD_LENGTH;

interface DeletionQueueMember {
	userId: bigint;
	payload: string;
}

export function parseDeletionQueueUserId(value: string): bigint {
	if (value.length === 0 || value.length > MAX_USER_ID_LENGTH || /\D/.test(value)) {
		throw new TypeError('Deletion queue user ID must be a canonical snowflake');
	}
	const parsed = SnowflakeType.safeParse(value);
	if (!parsed.success) throw new TypeError('Deletion queue user ID must be a canonical snowflake');
	return parsed.data;
}

export function parseDeletionQueueMember(value: string): DeletionQueueMember {
	if (value.length > MAX_MEMBER_LENGTH) throw new TypeError('Deletion queue member exceeds the maximum length');
	const separator = value.indexOf('|');
	if (separator <= 0 || separator === value.length - 1 || value.indexOf('|', separator + 1) !== -1) {
		throw new TypeError('Deletion queue member must contain exactly two nonempty fields');
	}
	if (value.length - separator - 1 > MAX_PAYLOAD_LENGTH) {
		throw new TypeError('Deletion queue payload exceeds the maximum length');
	}
	return {
		userId: parseDeletionQueueUserId(value.slice(0, separator)),
		payload: value.slice(separator + 1),
	};
}
