// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {CLOSE_DM_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {
	type IARReportContextType,
	type IARRuleCategoryId,
	type IARRuleReasonId,
	RULE_REASONS_BY_CATEGORY,
} from '@app/features/moderation/components/report_modal/IARFlowUtils';
import type {
	IARActionCardConfig,
	IARActionHandlers,
	IARContext,
	IARCopyBlock,
	IARRadioOption,
	IARResolvedContext,
} from '@app/features/moderation/components/report_modal/IARModalTypes';
import {BLOCK_DESCRIPTOR} from '@app/features/moderation/utils/ModerationMessageDescriptors';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const FINISH_ACCOUNT_SETUP_FIRST_DESCRIPTOR = msg({
	message: 'Finish account setup first',
	comment: 'IAR modal: title of the account-not-ready notice.',
});
const CLAIM_AND_VERIFY_TO_REPORT_DESCRIPTOR = msg({
	message: 'Claim your account and verify your email to send reports.',
	comment: 'IAR modal: body of the account-not-ready notice.',
});
const WHICH_AREA_DESCRIPTOR = msg({
	message: 'What kind of rule was broken?',
	comment: 'IAR modal: category-step title for the platform-report path. User picks a broad area before drilling in.',
});
const WHICH_RULE_DESCRIPTOR = msg({
	message: 'Which rule was broken?',
	comment: 'IAR modal: reason-step title for the platform-report path after a category has been picked.',
});
const CATEGORY_TARGETED_HARM_LABEL_DESCRIPTOR = msg({
	message: 'Threats, harassment, or harm',
	comment:
		'IAR modal: rule-category label grouping harassment, hate, violence, terrorism, raids, and self-harm content.',
});
const CATEGORY_TARGETED_HARM_DESC_DESCRIPTOR = msg({
	message: 'Bullying, threats, hate, violence, raids, or content that pushes self-harm.',
	comment: 'IAR modal: rule-category description for the targeted-harm group.',
});
const CATEGORY_SAFETY_MINORS_LABEL_DESCRIPTOR = msg({
	message: 'Child safety or mature content',
	comment: 'IAR modal: rule-category label grouping child-safety and mature-content reports.',
});
const CATEGORY_SAFETY_MINORS_DESC_DESCRIPTOR = msg({
	message: 'Minors at risk, mature content in the wrong place, or unwanted conduct.',
	comment: 'IAR modal: rule-category description for the child-safety / mature-content group.',
});
const CATEGORY_PRIVACY_IDENTITY_LABEL_DESCRIPTOR = msg({
	message: 'Privacy or impersonation',
	comment: 'IAR modal: rule-category label grouping privacy violations and impersonation.',
});
const CATEGORY_PRIVACY_IDENTITY_DESC_DESCRIPTOR = msg({
	message: 'Doxxing, stalking, pretending to be someone, or an inappropriate profile.',
	comment: 'IAR modal: rule-category description for the privacy / impersonation group.',
});
const CATEGORY_DECEPTION_LABEL_DESCRIPTOR = msg({
	message: 'Scams, malware, or misinformation',
	comment: 'IAR modal: rule-category label grouping spam/scams, malware, and harmful misinformation.',
});
const CATEGORY_DECEPTION_DESC_DESCRIPTOR = msg({
	message: 'Phishing, fraud, malicious links, or false claims likely to cause real-world harm.',
	comment: 'IAR modal: rule-category description for the scams / malware / misinformation group.',
});
const CATEGORY_ILLEGAL_OTHER_LABEL_DESCRIPTOR = msg({
	message: 'Illegal activity or something else',
	comment: 'IAR modal: rule-category label grouping illegal activity and the catch-all "Other" option.',
});
const CATEGORY_ILLEGAL_OTHER_DESC_DESCRIPTOR = msg({
	message: "Illegal sales, criminal facilitation, or a clear rule violation that doesn't fit above.",
	comment: 'IAR modal: rule-category description for the illegal / other group.',
});
const CHILD_SAFETY_MESSAGE_USER_DESCRIPTOR = msg({
	message: 'Child safety or exploitation of minors',
	comment: 'IAR modal: child-safety rule label (message and guild reports).',
});
const CHILD_SAFETY_USER_DESCRIPTOR = msg({
	message: 'Underage or child-safety concern',
	comment: 'IAR modal: child-safety rule label (user reports).',
});
const USE_CHILD_SAFETY_INSTEAD_DESCRIPTOR = msg({
	message: 'If a minor is involved, use "{childSafetyReasonName}" instead.',
	comment: 'IAR modal: inline routing hint shown under reasons that overlap with child safety.',
});
const REPORT_SENT_TITLE_DESCRIPTOR = msg({
	message: 'Report sent',
	comment: 'IAR modal: title on the success step shown after a platform report is submitted.',
});
const REPORT_SENT_BODY_DESCRIPTOR = msg({
	message: "Our safety team is reviewing it. We'll send you a DM and email once we've reached a verdict.",
	comment:
		'IAR modal: body copy on the success step shown after a platform report is submitted. Reassures the user about follow-up.',
});
const DELETE_MESSAGE_LABEL_DESCRIPTOR = msg({
	message: 'Delete this message',
	comment: 'IAR modal: quick-action card title for deleting the reported message (moderator-only).',
});
const DELETE_MESSAGE_DESC_DESCRIPTOR = msg({
	message: 'Remove it from the channel for everyone.',
	comment: 'IAR modal: quick-action card description for deleting the reported message.',
});
const DELETE_LABEL_DESCRIPTOR = msg({
	message: 'Delete',
	comment: 'IAR modal: quick-action card button label for deleting the reported message.',
});
const DELETED_LABEL_DESCRIPTOR = msg({
	message: 'Deleted',
	comment: 'IAR modal: disabled-state label on the delete card after the message has been deleted.',
});
const ALREADY_DELETED_TOOLTIP_DESCRIPTOR = msg({
	message: 'This message has already been deleted.',
	comment: 'IAR modal: tooltip on the delete card when the reported message is already deleted.',
});
const BAN_USER_LABEL_DESCRIPTOR = msg({
	message: 'Ban this user',
	comment: 'IAR modal: quick-action card title for banning the reported user (moderator-only).',
});
const BAN_USER_DESC_DESCRIPTOR = msg({
	message: 'Open the ban dialog for this community.',
	comment: 'IAR modal: quick-action card description for banning the reported user.',
});
const BAN_LABEL_DESCRIPTOR = msg({
	message: 'Ban',
	comment: 'IAR modal: quick-action card button label for opening the ban dialog.',
});
const BANNED_LABEL_DESCRIPTOR = msg({
	message: 'Banned',
	comment: 'IAR modal: disabled-state label on the ban card after the user is banned from this community.',
});
const ALREADY_BANNED_TOOLTIP_DESCRIPTOR = msg({
	message: 'This user is already banned from the community.',
	comment: 'IAR modal: tooltip on the ban card when the reported user is already banned.',
});
const HARASSMENT_OR_THREATS_DESCRIPTOR = msg({
	message: 'Harassment or threats',
	comment: 'IAR modal: rule label.',
});
const HARASSMENT_MESSAGE_DESC_DESCRIPTOR = msg({
	message: 'Bullying, repeated unwanted contact, stalking, or targeted abuse.',
	comment: 'IAR modal: rule description for a message report.',
});
const HARASSMENT_USER_LABEL_DESCRIPTOR = msg({
	message: 'Harassment or stalking',
	comment: 'IAR modal: rule label for a user report.',
});
const HARASSMENT_USER_DESC_DESCRIPTOR = msg({
	message: 'Threats, following across spaces, or unwanted contact.',
	comment: 'IAR modal: rule description for a user report.',
});
const HARASSMENT_GUILD_LABEL_DESCRIPTOR = msg({
	message: 'Harassment or targeted abuse',
	comment: 'IAR modal: rule label for a community report.',
});
const HARASSMENT_GUILD_DESC_DESCRIPTOR = msg({
	message: 'Community facilitates pile-ons or targeted abuse.',
	comment: 'IAR modal: rule description for a community report.',
});
const HATE_SPEECH_DESCRIPTOR = msg({
	message: 'Hate speech',
	comment: 'IAR modal: rule label.',
});
const HATE_MESSAGE_DESC_DESCRIPTOR = msg({
	message: 'Slurs, dehumanizing language, or attacks on protected groups.',
	comment: 'IAR modal: rule description.',
});
const HATE_USER_DESC_DESCRIPTOR = msg({
	message: 'Targets people by protected characteristics.',
	comment: 'IAR modal: rule description.',
});
const HATE_GUILD_DESC_DESCRIPTOR = msg({
	message: 'Promotes hatred against protected groups.',
	comment: 'IAR modal: rule description.',
});
const VIOLENCE_LABEL_DESCRIPTOR = msg({
	message: 'Violence or violent threats',
	comment: 'IAR modal: rule label.',
});
const VIOLENCE_DESC_DESCRIPTOR = msg({
	message: 'Credible threats, graphic violence, or glorification of violence.',
	comment: 'IAR modal: rule description.',
});
const TERRORISM_LABEL_DESCRIPTOR = msg({
	message: 'Terrorism or violent extremism',
	comment: 'IAR modal: rule label.',
});
const TERRORISM_DESC_DESCRIPTOR = msg({
	message: 'Promotes, recruits for, or coordinates violent extremist activity.',
	comment: 'IAR modal: rule description.',
});
const MATURE_CONTENT_LABEL_DESCRIPTOR = msg({
	message: 'Mature content or harassment',
	comment: 'IAR modal: rule label.',
});
const MATURE_CONTENT_MESSAGE_DESC_DESCRIPTOR = msg({
	message: 'Unwanted conduct or mature content in the wrong place.',
	comment: 'IAR modal: rule description.',
});
const MATURE_HARASSMENT_LABEL_DESCRIPTOR = msg({
	message: 'Harassment involving mature content',
	comment: 'IAR modal: rule label.',
});
const MATURE_HARASSMENT_USER_DESC_DESCRIPTOR = msg({
	message: 'Unwanted mature conduct, comments, or requests.',
	comment: 'IAR modal: rule description.',
});
const MATURE_CONTENT_GUILD_LABEL_DESCRIPTOR = msg({
	message: 'Mature content or unsafe gating',
	comment: 'IAR modal: rule label.',
});
const MATURE_CONTENT_GUILD_DESC_DESCRIPTOR = msg({
	message: 'Mature content without proper gating.',
	comment: 'IAR modal: rule description.',
});
const CHILD_SAFETY_MESSAGE_DESC_DESCRIPTOR = msg({
	message: 'Grooming or child-exploitation content.',
	comment: 'IAR modal: rule description.',
});
const CHILD_SAFETY_USER_DESC_DESCRIPTOR = msg({
	message: 'User appears underage, or is involved in grooming or child exploitation.',
	comment: 'IAR modal: rule description.',
});
const CHILD_SAFETY_GUILD_DESC_DESCRIPTOR = msg({
	message: 'Endangers minors or hosts child-exploitation content.',
	comment: 'IAR modal: rule description.',
});
const HARMFUL_MISINFO_LABEL_DESCRIPTOR = msg({
	message: 'Harmful misinformation',
	comment: 'IAR modal: rule label.',
});
const HARMFUL_MISINFO_DESC_DESCRIPTOR = msg({
	message: 'False claims likely to cause real-world harm.',
	comment: 'IAR modal: rule description.',
});
const SPAM_LABEL_DESCRIPTOR = msg({
	message: 'Spam, scams, or phishing',
	comment: 'IAR modal: rule label.',
});
const SPAM_MESSAGE_DESC_DESCRIPTOR = msg({
	message: 'Mass spam, fraud, fake giveaways, or account abuse.',
	comment: 'IAR modal: rule description.',
});
const SPAM_USER_LABEL_DESCRIPTOR = msg({
	message: 'Spam, scam, or phishing account',
	comment: 'IAR modal: rule label.',
});
const SPAM_USER_DESC_DESCRIPTOR = msg({
	message: 'Account exists to spam, scam, or abuse the platform.',
	comment: 'IAR modal: rule description.',
});
const SPAM_GUILD_DESC_DESCRIPTOR = msg({
	message: 'Community exists to spam, scam, or abuse the platform.',
	comment: 'IAR modal: rule description.',
});
const MALWARE_LABEL_DESCRIPTOR = msg({
	message: 'Malware or dangerous links',
	comment: 'IAR modal: rule label.',
});
const MALWARE_DESC_DESCRIPTOR = msg({
	message: 'Malware, credential theft, or harmful files.',
	comment: 'IAR modal: rule description.',
});
const MALWARE_USER_LABEL_DESCRIPTOR = msg({
	message: 'Sending malware or dangerous links',
	comment: 'IAR modal: rule label.',
});
const MALWARE_GUILD_LABEL_DESCRIPTOR = msg({
	message: 'Malware distribution',
	comment: 'IAR modal: rule label.',
});
const MALWARE_GUILD_DESC_DESCRIPTOR = msg({
	message: 'Distributes malware, credential theft, or harmful files.',
	comment: 'IAR modal: rule description.',
});
const PRIVACY_LABEL_DESCRIPTOR = msg({
	message: 'Privacy violation',
	comment: 'IAR modal: rule label.',
});
const PRIVACY_DESC_DESCRIPTOR = msg({
	message: 'Doxxing, exposed private info, or stalking.',
	comment: 'IAR modal: rule description.',
});
const PRIVACY_USER_LABEL_DESCRIPTOR = msg({
	message: 'Privacy violation or stalking',
	comment: 'IAR modal: rule label.',
});
const PRIVACY_GUILD_LABEL_DESCRIPTOR = msg({
	message: 'Privacy violation or doxxing',
	comment: 'IAR modal: rule label.',
});
const PRIVACY_GUILD_DESC_DESCRIPTOR = msg({
	message: 'Shares personal info, stalks users, or coordinates privacy abuse.',
	comment: 'IAR modal: rule description.',
});
const IMPERSONATION_LABEL_DESCRIPTOR = msg({
	message: 'Impersonation',
	comment: 'IAR modal: rule label.',
});
const IMPERSONATION_MESSAGE_LABEL_DESCRIPTOR = msg({
	message: 'Impersonation or deceptive media',
	comment: 'IAR modal: rule label.',
});
const IMPERSONATION_MESSAGE_DESC_DESCRIPTOR = msg({
	message: 'Pretending to be someone else, including deceptive AI-generated content.',
	comment: 'IAR modal: rule description.',
});
const IMPERSONATION_USER_DESC_DESCRIPTOR = msg({
	message: 'Claiming to be another person, brand, or organization.',
	comment: 'IAR modal: rule description.',
});
const PROFILE_VIOLATION_LABEL_DESCRIPTOR = msg({
	message: 'Profile, username, or avatar violation',
	comment: 'IAR modal: rule label.',
});
const PROFILE_VIOLATION_DESC_DESCRIPTOR = msg({
	message: "Violation is in the user's profile, not a specific message.",
	comment: 'IAR modal: rule description.',
});
const ILLEGAL_LABEL_DESCRIPTOR = msg({
	message: 'Illegal activity',
	comment: 'IAR modal: rule label.',
});
const ILLEGAL_DESC_DESCRIPTOR = msg({
	message: 'Illegal sales, criminal facilitation, or unlawful activity.',
	comment: 'IAR modal: rule description.',
});
const RAID_LABEL_DESCRIPTOR = msg({
	message: 'Raid coordination',
	comment: 'IAR modal: rule label.',
});
const RAID_DESC_DESCRIPTOR = msg({
	message: 'Coordinates raids, brigading, or harassment against people or communities.',
	comment: 'IAR modal: rule description.',
});
const SELF_HARM_LABEL_DESCRIPTOR = msg({
	message: 'Self-harm or suicide',
	comment: 'IAR modal: rule label.',
});
const SELF_HARM_MESSAGE_DESC_DESCRIPTOR = msg({
	message: 'Promotion or instructions encouraging self-harm or eating disorders.',
	comment: 'IAR modal: rule description.',
});
const SELF_HARM_USER_LABEL_DESCRIPTOR = msg({
	message: 'Encourages self-harm',
	comment: 'IAR modal: rule label.',
});
const SELF_HARM_USER_DESC_DESCRIPTOR = msg({
	message: 'Encourages suicide, self-harm, or eating disorders.',
	comment: 'IAR modal: rule description.',
});
const OTHER_LABEL_DESCRIPTOR = msg({
	message: 'Another clear rule violation',
	comment: 'IAR modal: rule label.',
});
const OTHER_DESC_DESCRIPTOR = msg({
	message: "Use only if it clearly breaks {productName}'s rules and doesn't fit above.",
	comment: 'IAR modal: rule description.',
});
const CSAM_SAFETY_NOTE_DESCRIPTOR = msg({
	message: "If this involves CSAM or exploitation of a minor, send it now and don't reshare the material.",
	comment: 'IAR modal: inline safety note shown for child-safety reports.',
});
const SELF_HARM_SAFETY_NOTE_DESCRIPTOR = msg({
	message: 'If someone may be in immediate danger, contact local emergency services if you can do so safely.',
	comment: 'IAR modal: inline safety note for self-harm reports.',
});
const VIOLENCE_SAFETY_NOTE_DESCRIPTOR = msg({
	message: 'If this is a credible imminent threat, contact local emergency services too.',
	comment: 'IAR modal: inline safety note for violence reports.',
});
const TERRORISM_SAFETY_NOTE_DESCRIPTOR = msg({
	message: 'If this is an imminent terrorist threat, contact local emergency services too.',
	comment: 'IAR modal: inline safety note for terrorism reports.',
});
const BLOCK_USER_LABEL_DESCRIPTOR = msg({
	message: 'Block this user',
	comment: 'IAR modal: quick-action card title.',
});
const BLOCK_USER_DESC_DESCRIPTOR = msg({
	message: 'Stop messages and friend requests.',
	comment: 'IAR modal: quick-action card description.',
});
const COPY_MESSAGE_LINK_LABEL_DESCRIPTOR = msg({
	message: 'Copy message link',
	comment: 'IAR modal: quick-action card title.',
});
const COPY_MESSAGE_LINK_DESC_DESCRIPTOR = msg({
	message: 'Share with community mods.',
	comment: 'IAR modal: quick-action card description.',
});
const COPY_DESCRIPTOR = msg({
	message: 'Copy',
	comment: 'IAR modal: quick-action card button label.',
});
const CLOSE_DM_LABEL_DESCRIPTOR = msg({
	message: 'Close this DM',
	comment: 'IAR modal: quick-action card title.',
});
const CLOSE_DM_DESC_DESCRIPTOR = msg({
	message: "Doesn't block. You can reopen later.",
	comment: 'IAR modal: quick-action card description.',
});
const LEAVE_COMMUNITY_LABEL_DESCRIPTOR = msg({
	message: 'Leave the community',
	comment: 'IAR modal: quick-action card title.',
});
const LEAVE_COMMUNITY_DESC_DESCRIPTOR = msg({
	message: 'Stop seeing its content and members.',
	comment: 'IAR modal: quick-action card description.',
});
const LEAVE_DESCRIPTOR = msg({
	message: 'Leave',
	comment: 'IAR modal: quick-action card button label.',
});
const DM_SETTINGS_LABEL_DESCRIPTOR = msg({
	message: 'DM & friend request settings',
	comment: 'IAR modal: quick-action card title.',
});
const DM_SETTINGS_DESC_DESCRIPTOR = msg({
	message: 'Change who can reach you.',
	comment: 'IAR modal: quick-action card description.',
});
const OPEN_DESCRIPTOR = msg({
	message: 'Open',
	comment: 'IAR modal: quick-action card button label.',
});
const CALL_SETTINGS_LABEL_DESCRIPTOR = msg({
	message: 'Call & group chat settings',
	comment: 'IAR modal: quick-action card title.',
});
const CALL_SETTINGS_DESC_DESCRIPTOR = msg({
	message: 'Change who can call or add you.',
	comment: 'IAR modal: quick-action card description.',
});
const MODAL_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Report a rule violation, or find tools to manage contact and preferences.',
	comment: 'IAR modal: screen-reader description for the whole modal.',
});

function shouldShowContactSettings(context: IARContext): boolean {
	return context.type !== 'guild';
}

export function getIARModalDescription(i18n: I18n): string {
	return i18n._(MODAL_DESCRIPTION_DESCRIPTOR);
}

export function getIARReportEligibilityCopy(i18n: I18n): IARCopyBlock {
	return {
		title: i18n._(FINISH_ACCOUNT_SETUP_FIRST_DESCRIPTOR),
		body: i18n._(CLAIM_AND_VERIFY_TO_REPORT_DESCRIPTOR),
	};
}

export function getIARReasonTitle(i18n: I18n): string {
	return i18n._(WHICH_RULE_DESCRIPTOR);
}

export function getIARCategoryTitle(i18n: I18n): string {
	return i18n._(WHICH_AREA_DESCRIPTOR);
}

export function getIARRuleCategoryOptions(i18n: I18n): Array<IARRadioOption<IARRuleCategoryId>> {
	return [
		{
			value: 'targeted_harm',
			name: i18n._(CATEGORY_TARGETED_HARM_LABEL_DESCRIPTOR),
			desc: i18n._(CATEGORY_TARGETED_HARM_DESC_DESCRIPTOR),
		},
		{
			value: 'safety_minors',
			name: i18n._(CATEGORY_SAFETY_MINORS_LABEL_DESCRIPTOR),
			desc: i18n._(CATEGORY_SAFETY_MINORS_DESC_DESCRIPTOR),
		},
		{
			value: 'privacy_identity',
			name: i18n._(CATEGORY_PRIVACY_IDENTITY_LABEL_DESCRIPTOR),
			desc: i18n._(CATEGORY_PRIVACY_IDENTITY_DESC_DESCRIPTOR),
		},
		{
			value: 'deception',
			name: i18n._(CATEGORY_DECEPTION_LABEL_DESCRIPTOR),
			desc: i18n._(CATEGORY_DECEPTION_DESC_DESCRIPTOR),
		},
		{
			value: 'illegal_other',
			name: i18n._(CATEGORY_ILLEGAL_OTHER_LABEL_DESCRIPTOR),
			desc: i18n._(CATEGORY_ILLEGAL_OTHER_DESC_DESCRIPTOR),
		},
	];
}

function getChildSafetyReasonName(i18n: I18n, contextType: IARReportContextType): string {
	switch (contextType) {
		case 'message':
		case 'guild':
			return i18n._(CHILD_SAFETY_MESSAGE_USER_DESCRIPTOR);
		case 'user':
			return i18n._(CHILD_SAFETY_USER_DESCRIPTOR);
	}
}

export function getIARChildSafetyRoutingNote(
	i18n: I18n,
	contextType: IARReportContextType,
	selectedReason: IARRuleReasonId | null,
): string | null {
	if (selectedReason === null) {
		return null;
	}
	switch (selectedReason) {
		case 'illegal_activity':
		case 'mature_content':
		case 'other':
			return i18n._(USE_CHILD_SAFETY_INSTEAD_DESCRIPTOR, {
				childSafetyReasonName: getChildSafetyReasonName(i18n, contextType),
			});
		case 'child_safety':
		case 'harassment':
		case 'hate':
		case 'violence':
		case 'harmful_misinformation':
		case 'spam_scams':
		case 'malware':
		case 'privacy':
		case 'impersonation':
		case 'self_harm':
		case 'terrorism_extremism':
		case 'inappropriate_profile':
		case 'raid_coordination':
			return null;
	}
}

function getMessageRuleReasonOptions(i18n: I18n): Array<IARRadioOption<IARRuleReasonId>> {
	return [
		{
			value: 'harassment',
			name: i18n._(HARASSMENT_OR_THREATS_DESCRIPTOR),
			desc: i18n._(HARASSMENT_MESSAGE_DESC_DESCRIPTOR),
		},
		{value: 'hate', name: i18n._(HATE_SPEECH_DESCRIPTOR), desc: i18n._(HATE_MESSAGE_DESC_DESCRIPTOR)},
		{value: 'violence', name: i18n._(VIOLENCE_LABEL_DESCRIPTOR), desc: i18n._(VIOLENCE_DESC_DESCRIPTOR)},
		{
			value: 'mature_content',
			name: i18n._(MATURE_CONTENT_LABEL_DESCRIPTOR),
			desc: i18n._(MATURE_CONTENT_MESSAGE_DESC_DESCRIPTOR),
		},
		{
			value: 'child_safety',
			name: i18n._(CHILD_SAFETY_MESSAGE_USER_DESCRIPTOR),
			desc: i18n._(CHILD_SAFETY_MESSAGE_DESC_DESCRIPTOR),
		},
		{
			value: 'harmful_misinformation',
			name: i18n._(HARMFUL_MISINFO_LABEL_DESCRIPTOR),
			desc: i18n._(HARMFUL_MISINFO_DESC_DESCRIPTOR),
		},
		{value: 'spam_scams', name: i18n._(SPAM_LABEL_DESCRIPTOR), desc: i18n._(SPAM_MESSAGE_DESC_DESCRIPTOR)},
		{value: 'malware', name: i18n._(MALWARE_LABEL_DESCRIPTOR), desc: i18n._(MALWARE_DESC_DESCRIPTOR)},
		{value: 'privacy', name: i18n._(PRIVACY_LABEL_DESCRIPTOR), desc: i18n._(PRIVACY_DESC_DESCRIPTOR)},
		{
			value: 'impersonation',
			name: i18n._(IMPERSONATION_MESSAGE_LABEL_DESCRIPTOR),
			desc: i18n._(IMPERSONATION_MESSAGE_DESC_DESCRIPTOR),
		},
		{value: 'illegal_activity', name: i18n._(ILLEGAL_LABEL_DESCRIPTOR), desc: i18n._(ILLEGAL_DESC_DESCRIPTOR)},
		{value: 'self_harm', name: i18n._(SELF_HARM_LABEL_DESCRIPTOR), desc: i18n._(SELF_HARM_MESSAGE_DESC_DESCRIPTOR)},
		{
			value: 'other',
			name: i18n._(OTHER_LABEL_DESCRIPTOR),
			desc: i18n._(OTHER_DESC_DESCRIPTOR, {productName: PRODUCT_NAME}),
		},
	];
}

function getUserRuleReasonOptions(i18n: I18n): Array<IARRadioOption<IARRuleReasonId>> {
	return [
		{
			value: 'harassment',
			name: i18n._(HARASSMENT_USER_LABEL_DESCRIPTOR),
			desc: i18n._(HARASSMENT_USER_DESC_DESCRIPTOR),
		},
		{value: 'hate', name: i18n._(HATE_SPEECH_DESCRIPTOR), desc: i18n._(HATE_USER_DESC_DESCRIPTOR)},
		{
			value: 'mature_content',
			name: i18n._(MATURE_HARASSMENT_LABEL_DESCRIPTOR),
			desc: i18n._(MATURE_HARASSMENT_USER_DESC_DESCRIPTOR),
		},
		{
			value: 'child_safety',
			name: i18n._(CHILD_SAFETY_USER_DESCRIPTOR),
			desc: i18n._(CHILD_SAFETY_USER_DESC_DESCRIPTOR),
		},
		{
			value: 'harmful_misinformation',
			name: i18n._(HARMFUL_MISINFO_LABEL_DESCRIPTOR),
			desc: i18n._(HARMFUL_MISINFO_DESC_DESCRIPTOR),
		},
		{value: 'spam_scams', name: i18n._(SPAM_USER_LABEL_DESCRIPTOR), desc: i18n._(SPAM_USER_DESC_DESCRIPTOR)},
		{value: 'malware', name: i18n._(MALWARE_USER_LABEL_DESCRIPTOR), desc: i18n._(MALWARE_DESC_DESCRIPTOR)},
		{value: 'privacy', name: i18n._(PRIVACY_USER_LABEL_DESCRIPTOR), desc: i18n._(PRIVACY_DESC_DESCRIPTOR)},
		{
			value: 'impersonation',
			name: i18n._(IMPERSONATION_LABEL_DESCRIPTOR),
			desc: i18n._(IMPERSONATION_USER_DESC_DESCRIPTOR),
		},
		{
			value: 'inappropriate_profile',
			name: i18n._(PROFILE_VIOLATION_LABEL_DESCRIPTOR),
			desc: i18n._(PROFILE_VIOLATION_DESC_DESCRIPTOR),
		},
		{value: 'illegal_activity', name: i18n._(ILLEGAL_LABEL_DESCRIPTOR), desc: i18n._(ILLEGAL_DESC_DESCRIPTOR)},
		{value: 'self_harm', name: i18n._(SELF_HARM_USER_LABEL_DESCRIPTOR), desc: i18n._(SELF_HARM_USER_DESC_DESCRIPTOR)},
		{
			value: 'other',
			name: i18n._(OTHER_LABEL_DESCRIPTOR),
			desc: i18n._(OTHER_DESC_DESCRIPTOR, {productName: PRODUCT_NAME}),
		},
	];
}

function getGuildRuleReasonOptions(i18n: I18n): Array<IARRadioOption<IARRuleReasonId>> {
	return [
		{
			value: 'harassment',
			name: i18n._(HARASSMENT_GUILD_LABEL_DESCRIPTOR),
			desc: i18n._(HARASSMENT_GUILD_DESC_DESCRIPTOR),
		},
		{value: 'hate', name: i18n._(HATE_SPEECH_DESCRIPTOR), desc: i18n._(HATE_GUILD_DESC_DESCRIPTOR)},
		{value: 'terrorism_extremism', name: i18n._(TERRORISM_LABEL_DESCRIPTOR), desc: i18n._(TERRORISM_DESC_DESCRIPTOR)},
		{
			value: 'mature_content',
			name: i18n._(MATURE_CONTENT_GUILD_LABEL_DESCRIPTOR),
			desc: i18n._(MATURE_CONTENT_GUILD_DESC_DESCRIPTOR),
		},
		{
			value: 'child_safety',
			name: i18n._(CHILD_SAFETY_MESSAGE_USER_DESCRIPTOR),
			desc: i18n._(CHILD_SAFETY_GUILD_DESC_DESCRIPTOR),
		},
		{
			value: 'harmful_misinformation',
			name: i18n._(HARMFUL_MISINFO_LABEL_DESCRIPTOR),
			desc: i18n._(HARMFUL_MISINFO_DESC_DESCRIPTOR),
		},
		{value: 'raid_coordination', name: i18n._(RAID_LABEL_DESCRIPTOR), desc: i18n._(RAID_DESC_DESCRIPTOR)},
		{value: 'spam_scams', name: i18n._(SPAM_LABEL_DESCRIPTOR), desc: i18n._(SPAM_GUILD_DESC_DESCRIPTOR)},
		{value: 'malware', name: i18n._(MALWARE_GUILD_LABEL_DESCRIPTOR), desc: i18n._(MALWARE_GUILD_DESC_DESCRIPTOR)},
		{value: 'privacy', name: i18n._(PRIVACY_GUILD_LABEL_DESCRIPTOR), desc: i18n._(PRIVACY_GUILD_DESC_DESCRIPTOR)},
		{value: 'illegal_activity', name: i18n._(ILLEGAL_LABEL_DESCRIPTOR), desc: i18n._(ILLEGAL_DESC_DESCRIPTOR)},
		{value: 'self_harm', name: i18n._(SELF_HARM_USER_LABEL_DESCRIPTOR), desc: i18n._(SELF_HARM_USER_DESC_DESCRIPTOR)},
		{
			value: 'other',
			name: i18n._(OTHER_LABEL_DESCRIPTOR),
			desc: i18n._(OTHER_DESC_DESCRIPTOR, {productName: PRODUCT_NAME}),
		},
	];
}

export function getIARRuleReasonOptions(
	i18n: I18n,
	contextType: IARReportContextType,
	category?: IARRuleCategoryId | null,
): Array<IARRadioOption<IARRuleReasonId>> {
	const all = (() => {
		switch (contextType) {
			case 'message':
				return getMessageRuleReasonOptions(i18n);
			case 'user':
				return getUserRuleReasonOptions(i18n);
			case 'guild':
				return getGuildRuleReasonOptions(i18n);
		}
	})();
	if (!category) return all;
	const allowed = RULE_REASONS_BY_CATEGORY[category];
	return all.filter((opt) => allowed.includes(opt.value));
}

export function getIARSpecialSafetyNote(i18n: I18n, selectedReason: IARRuleReasonId | null): string | null {
	if (selectedReason === null) {
		return null;
	}
	switch (selectedReason) {
		case 'child_safety':
			return i18n._(CSAM_SAFETY_NOTE_DESCRIPTOR);
		case 'self_harm':
			return i18n._(SELF_HARM_SAFETY_NOTE_DESCRIPTOR);
		case 'violence':
			return i18n._(VIOLENCE_SAFETY_NOTE_DESCRIPTOR);
		case 'terrorism_extremism':
			return i18n._(TERRORISM_SAFETY_NOTE_DESCRIPTOR);
		case 'harassment':
		case 'hate':
		case 'mature_content':
		case 'illegal_activity':
		case 'spam_scams':
		case 'malware':
		case 'privacy':
		case 'impersonation':
		case 'inappropriate_profile':
		case 'harmful_misinformation':
		case 'raid_coordination':
		case 'other':
			return null;
	}
}

export interface IARActionCardsOptions {
	isMessageDeleted?: boolean;
	isUserBanned?: boolean;
}

export function getIARActionCards(
	i18n: I18n,
	context: IARContext,
	resolvedContext: IARResolvedContext,
	handlers: IARActionHandlers,
	options: IARActionCardsOptions = {},
): Array<IARActionCardConfig> {
	const cards: Array<IARActionCardConfig> = [];
	const {isMessageDeleted = false, isUserBanned = false} = options;
	if (!resolvedContext.isReportedUserBlocked && resolvedContext.reportedUser !== null) {
		cards.push({
			id: 'block-user',
			title: i18n._(BLOCK_USER_LABEL_DESCRIPTOR),
			description: i18n._(BLOCK_USER_DESC_DESCRIPTOR),
			label: i18n._(BLOCK_DESCRIPTOR),
			buttonVariant: 'danger',
			onClick: handlers.onBlockUser,
		});
	}
	if (context.type === 'message' && resolvedContext.hasCommunityContext) {
		cards.push({
			id: 'copy-message-link',
			title: i18n._(COPY_MESSAGE_LINK_LABEL_DESCRIPTOR),
			description: i18n._(COPY_MESSAGE_LINK_DESC_DESCRIPTOR),
			label: i18n._(COPY_DESCRIPTOR),
			buttonVariant: 'secondary',
			onClick: handlers.onCopyMessageLink,
		});
	}
	if (resolvedContext.dmChannel !== null && resolvedContext.isFocusedOnDMWithUser) {
		cards.push({
			id: 'close-dm',
			title: i18n._(CLOSE_DM_LABEL_DESCRIPTOR),
			description: i18n._(CLOSE_DM_DESC_DESCRIPTOR),
			label: i18n._(CLOSE_DM_DESCRIPTOR),
			buttonVariant: 'danger',
			onClick: handlers.onCloseDM,
		});
	}
	if (resolvedContext.leaveableGuildId !== null && !resolvedContext.isLeaveableGuildOwner) {
		cards.push({
			id: 'leave-community',
			title: i18n._(LEAVE_COMMUNITY_LABEL_DESCRIPTOR),
			description: i18n._(LEAVE_COMMUNITY_DESC_DESCRIPTOR),
			label: i18n._(LEAVE_DESCRIPTOR),
			buttonVariant: 'danger',
			onClick: handlers.onLeaveCommunity,
		});
	}
	if (resolvedContext.canDeleteReportedMessage) {
		const deleteDisabled = isMessageDeleted;
		cards.push({
			id: 'delete-message',
			title: i18n._(DELETE_MESSAGE_LABEL_DESCRIPTOR),
			description: i18n._(DELETE_MESSAGE_DESC_DESCRIPTOR),
			label: deleteDisabled ? i18n._(DELETED_LABEL_DESCRIPTOR) : i18n._(DELETE_LABEL_DESCRIPTOR),
			buttonVariant: 'danger',
			onClick: handlers.onDeleteMessage,
			disabled: deleteDisabled,
			disabledTooltip: deleteDisabled ? i18n._(ALREADY_DELETED_TOOLTIP_DESCRIPTOR) : undefined,
		});
	}
	if (resolvedContext.canBanReportedUser) {
		const banDisabled = isUserBanned;
		cards.push({
			id: 'ban-user',
			title: i18n._(BAN_USER_LABEL_DESCRIPTOR),
			description: i18n._(BAN_USER_DESC_DESCRIPTOR),
			label: banDisabled ? i18n._(BANNED_LABEL_DESCRIPTOR) : i18n._(BAN_LABEL_DESCRIPTOR),
			buttonVariant: 'danger',
			onClick: handlers.onBanUser,
			disabled: banDisabled,
			disabledTooltip: banDisabled ? i18n._(ALREADY_BANNED_TOOLTIP_DESCRIPTOR) : undefined,
		});
	}
	if (shouldShowContactSettings(context)) {
		cards.push({
			id: 'connections-settings',
			title: i18n._(DM_SETTINGS_LABEL_DESCRIPTOR),
			description: i18n._(DM_SETTINGS_DESC_DESCRIPTOR),
			label: i18n._(OPEN_DESCRIPTOR),
			buttonVariant: 'secondary',
			onClick: handlers.onOpenConnectionsSettings,
		});
		cards.push({
			id: 'communication-settings',
			title: i18n._(CALL_SETTINGS_LABEL_DESCRIPTOR),
			description: i18n._(CALL_SETTINGS_DESC_DESCRIPTOR),
			label: i18n._(OPEN_DESCRIPTOR),
			buttonVariant: 'secondary',
			onClick: handlers.onOpenCommunicationSettings,
		});
	}
	return cards;
}

export function getIARSuccessCopy(i18n: I18n): IARCopyBlock {
	return {
		title: i18n._(REPORT_SENT_TITLE_DESCRIPTOR),
		body: i18n._(REPORT_SENT_BODY_DESCRIPTOR),
	};
}
