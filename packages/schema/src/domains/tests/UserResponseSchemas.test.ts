// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	CustomStatusResponse,
	RelationshipResponse,
	UserPartialResponse,
	UserProfileFullResponse,
} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {describe, expect, it} from 'vitest';

describe('UserPartialResponse', () => {
	const validUser = {
		id: '123456789012345678',
		username: 'testuser',
		discriminator: '0001',
		global_name: 'Test User',
		avatar: 'avatar_hash',
		avatar_color: 0xff5500,
		flags: 0,
	};
	it('accepts valid user partial response', () => {
		const result = UserPartialResponse.parse(validUser);
		expect(result.id).toBe('123456789012345678');
		expect(result.username).toBe('testuser');
	});
	it('accepts null global_name and avatar', () => {
		const user = {
			...validUser,
			global_name: null,
			avatar: null,
			avatar_color: null,
		};
		const result = UserPartialResponse.safeParse(user);
		expect(result.success).toBe(true);
	});
	it('accepts optional bot flag', () => {
		const user = {...validUser, bot: true};
		const result = UserPartialResponse.parse(user);
		expect(result.bot).toBe(true);
	});
	it('accepts optional system flag', () => {
		const user = {...validUser, system: true};
		const result = UserPartialResponse.parse(user);
		expect(result.system).toBe(true);
	});
	it('requires id', () => {
		const {id, ...userWithoutId} = validUser;
		const result = UserPartialResponse.safeParse(userWithoutId);
		expect(result.success).toBe(false);
	});
	it('requires username', () => {
		const {username, ...userWithoutUsername} = validUser;
		const result = UserPartialResponse.safeParse(userWithoutUsername);
		expect(result.success).toBe(false);
	});
	it('requires discriminator', () => {
		const {discriminator, ...userWithoutDiscriminator} = validUser;
		const result = UserPartialResponse.safeParse(userWithoutDiscriminator);
		expect(result.success).toBe(false);
	});
	it('requires flags', () => {
		const {flags, ...userWithoutFlags} = validUser;
		const result = UserPartialResponse.safeParse(userWithoutFlags);
		expect(result.success).toBe(false);
	});
});

describe('UserProfileFullResponse', () => {
	const user = {
		id: '123456789012345678',
		username: 'testuser',
		discriminator: '0001',
		global_name: 'Test User',
		avatar: null,
		avatar_color: null,
		flags: 0,
	};
	const profile = {
		user,
		user_profile: {
			bio: null,
			pronouns: null,
			banner: null,
			banner_color: null,
			accent_color: null,
		},
		guild_member_profile: null,
		timezone_offset: -240,
	};
	it('accepts negative timezone offsets', () => {
		const result = UserProfileFullResponse.safeParse(profile);
		expect(result.success).toBe(true);
	});
	it('accepts null timezone offsets', () => {
		const result = UserProfileFullResponse.safeParse({...profile, timezone_offset: null});
		expect(result.success).toBe(true);
	});
});

describe('CustomStatusResponse', () => {
	it('accepts valid custom status', () => {
		const result = CustomStatusResponse.safeParse({
			text: 'Working on a project',
			expires_at: '2024-01-15T18:00:00.000Z',
			emoji_id: '123456789012345678',
			emoji_name: 'custom_emoji',
			emoji_animated: false,
		});
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data.text).toBe('Working on a project');
		}
	});
	it('accepts minimal custom status', () => {
		const result = CustomStatusResponse.safeParse({
			emoji_animated: false,
		});
		expect(result.success).toBe(true);
	});
	it('accepts null optional fields', () => {
		const result = CustomStatusResponse.safeParse({
			text: null,
			expires_at: null,
			emoji_id: null,
			emoji_name: null,
			emoji_animated: false,
		});
		expect(result.success).toBe(true);
	});
	it('accepts unicode emoji name', () => {
		const result = CustomStatusResponse.safeParse({
			emoji_name: '\uD83D\uDE00',
			emoji_animated: false,
		});
		expect(result.success).toBe(true);
	});
	it('requires emoji_animated', () => {
		const result = CustomStatusResponse.safeParse({
			text: 'Hello',
		});
		expect(result.success).toBe(false);
	});
	it('accepts animated emoji', () => {
		const result = CustomStatusResponse.safeParse({
			emoji_id: '123456789012345678',
			emoji_name: 'animated_emoji',
			emoji_animated: true,
		});
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data.emoji_animated).toBe(true);
		}
	});
});

describe('RelationshipResponse', () => {
	const validRelationship = {
		id: '123456789012345678',
		type: 1,
		user: {
			id: '987654321098765432',
			username: 'friend',
			discriminator: '0001',
			global_name: 'Friend',
			avatar: null,
			avatar_color: null,
			flags: 0,
		},
		nickname: 'Best Friend',
		share_voice_activity: true,
		friend_shares_voice_activity: true,
	};
	it('accepts valid relationship', () => {
		const result = RelationshipResponse.parse(validRelationship);
		expect(result.id).toBe('123456789012345678');
		expect(result.type).toBe(1);
	});
	it('accepts relationship with null nickname', () => {
		const relationship = {...validRelationship, nickname: null};
		const result = RelationshipResponse.safeParse(relationship);
		expect(result.success).toBe(true);
	});
	it('accepts relationship with since date', () => {
		const relationship = {...validRelationship, since: '2024-01-01T00:00:00.000Z'};
		const result = RelationshipResponse.safeParse(relationship);
		expect(result.success).toBe(true);
	});
	it('requires id', () => {
		const {id, ...relationshipWithoutId} = validRelationship;
		const result = RelationshipResponse.safeParse(relationshipWithoutId);
		expect(result.success).toBe(false);
	});
	it('requires type', () => {
		const {type, ...relationshipWithoutType} = validRelationship;
		const result = RelationshipResponse.safeParse(relationshipWithoutType);
		expect(result.success).toBe(false);
	});
	it('requires user', () => {
		const {user, ...relationshipWithoutUser} = validRelationship;
		const result = RelationshipResponse.safeParse(relationshipWithoutUser);
		expect(result.success).toBe(false);
	});
	it('validates nested user object', () => {
		const relationship = {
			...validRelationship,
			user: {id: 'invalid'},
		};
		const result = RelationshipResponse.safeParse(relationship);
		expect(result.success).toBe(false);
	});
	it('accepts different relationship types', () => {
		for (const type of [1, 2, 3, 4]) {
			const relationship = {...validRelationship, type};
			const result = RelationshipResponse.safeParse(relationship);
			expect(result.success).toBe(true);
		}
	});
});
