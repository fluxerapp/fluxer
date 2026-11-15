// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const ACCEPT_INVITE_DESCRIPTOR = msg({
	message: 'Accept invite',
	comment: 'Short title, accessible label, or button label for accepting an invite.',
});
export const INVITE_NOT_FOUND_TITLE_DESCRIPTOR = msg({
	message: 'Invite not found',
	comment: 'Title for invite errors when the invite code does not exist or cannot be used.',
});
export const INVITE_NOT_FOUND_DESCRIPTION_DESCRIPTOR = msg({
	message: 'This invite may have expired or been deleted.',
	comment: 'Invite error body shown when an invite code cannot be found or used.',
});
export const RAID_INVITES_PAUSED_DESCRIPTOR = msg({
	message: '{productName} detected a potential raid in this community, so invites are paused.',
	comment:
		'Invite page notice shown when automated raid protection pauses invites. productName is the Fluxer product name.',
});
export const RAID_INVITES_PAUSED_SHORT_DESCRIPTOR = msg({
	message:
		'{productName} detected a potential raid in this community. Invites are paused, so new users cannot join right now.',
	comment:
		'Invite modal notice shown when automated raid protection pauses invites. productName is the Fluxer product name.',
});
export const INVITES_PAUSED_BECAUSE_RAID_DESCRIPTOR = msg({
	message: "Invites are paused because {productName} detected a potential raid. New users can't join right now.",
	comment:
		'Invite settings notice shown when automated raid protection pauses invites. productName is the Fluxer product name.',
});
export const INVITES_PAUSED_DESCRIPTOR = msg({
	message: 'This community has paused invites.',
	comment: 'Invite page notice shown when community admins have paused invites.',
});
export const INVITES_PAUSED_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'This community has paused invites. You can try again later.',
	comment: 'Invite modal notice shown when community admins have paused invites.',
});
export const RAID_INVITES_PAUSED_ACCOUNT_ACTIONS_DESCRIPTOR = msg({
	message:
		"New users can't join right now. You can still create an account or sign in, then try this link again later.",
	comment:
		'Invite page subtext shown during raid protection. Explains that account actions still work, but joining is paused.',
});
export const INVITES_PAUSED_ACCOUNT_ACTIONS_DESCRIPTOR = msg({
	message:
		'You can still create an account or sign in. If invites are re-enabled later, you can use this same link to join.',
	comment: 'Invite page subtext shown when community admins pause invites. Explains that account actions still work.',
});
export const PREVIEW_COMMUNITY_DESCRIPTOR = msg({
	message: 'Preview',
	context: 'invite-preview-button',
	comment:
		'Invite button, a verb: starts previewing the community before joining it. Previewing means reading a community before joining. Joining happens from the bar at the top of the community.',
});
