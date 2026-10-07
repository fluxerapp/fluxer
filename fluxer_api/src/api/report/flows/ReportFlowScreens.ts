// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ContentI18nKey} from '@app/api/content_i18n/ContentI18nMessages';
import type {ReportReasonKey} from '@app/api/report/flows/ReportReasonCatalog';
import type {ReportFlowSurface} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';

export type ReportFlowCopyKey = Extract<ContentI18nKey, `report_flow.${string}`>;

export type ReportFlowLinkId = 'dsa' | 'copyright' | 'crisis_lines' | 'guidelines';

export type ReportFlowOutcomeDef =
	| {type: 'screen'; screenId: string}
	| {type: 'submit'; reason: ReportReasonKey}
	| {type: 'end'; noticeId?: string}
	| {type: 'link'; link: ReportFlowLinkId};

export interface ReportFlowOptionDef {
	id: string;
	label: ReportFlowCopyKey;
	outcome: ReportFlowOutcomeDef;
	surface?: ReportFlowSurface;
}

export interface ReportFlowChecklistItemDef {
	id: string;
	label: ReportFlowCopyKey;
	description?: ReportFlowCopyKey;
	reason?: ReportReasonKey;
}

export interface ReportFlowChecklistDef {
	items: ReadonlyArray<ReportFlowChecklistItemDef>;
	minChecked: number;
	outcome: Extract<ReportFlowOutcomeDef, {type: 'screen' | 'submit'}>;
}

export interface ReportFlowSubtitleVariantsDef {
	dsa?: ReportFlowCopyKey | null;
	selfHosted?: ReportFlowCopyKey | null;
}

export interface ReportFlowScreenDef {
	id: string;
	title: ReportFlowCopyKey;
	subtitle?: ReportFlowCopyKey;
	subtitleVariants?: ReportFlowSubtitleVariantsDef;
	urgent?: true;
	options?: ReadonlyArray<ReportFlowOptionDef>;
	optionsHeading?: ReportFlowCopyKey;
	checklist?: ReportFlowChecklistDef;
	nextScreenId?: string;
}

export interface ReportFlowNoticeDef {
	id: string;
	title: ReportFlowCopyKey;
	body: ReportFlowCopyKey;
}

export const REPORT_FLOW_NOTICES: ReadonlyArray<ReportFlowNoticeDef> = [
	{
		id: 'need_more_info',
		title: 'report_flow.notice.need_more_info.title',
		body: 'report_flow.notice.need_more_info.body',
	},
	{
		id: 'need_more_info_profile',
		title: 'report_flow.notice.need_more_info.title',
		body: 'report_flow.notice.need_more_info_profile.body',
	},
];

export const PRIVATE_INFO_ITEMS: ReadonlyArray<ReportFlowChecklistItemDef> = [
	{id: 'email', label: 'report_flow.label.email'},
	{id: 'phone', label: 'report_flow.label.phone'},
	{id: 'address', label: 'report_flow.label.address'},
	{id: 'legal_name', label: 'report_flow.label.legal_name'},
	{id: 'government_id', label: 'report_flow.label.government_id'},
	{id: 'face_photo', label: 'report_flow.label.face_photo'},
	{id: 'identity', label: 'report_flow.label.identity'},
	{id: 'financial', label: 'report_flow.label.financial'},
	{id: 'ip_address', label: 'report_flow.label.ip_address'},
	{id: 'intimate_photo', label: 'report_flow.label.intimate_photo', reason: 'intimate_image_abuse'},
	{id: 'private_conversation', label: 'report_flow.label.private_conversation'},
	{id: 'other_identifying', label: 'report_flow.label.other_identifying'},
	{id: 'threat_to_share', label: 'report_flow.label.threat_to_share'},
];

export const REPORT_FLOW_SCREENS: ReadonlyArray<ReportFlowScreenDef> = [
	{
		id: 'root_message',
		title: 'report_flow.screen.root_message.title',
		subtitle: 'report_flow.screen.root_message.subtitle',
		options: [
			{id: 'abuse', label: 'report_flow.label.abuse', outcome: {type: 'screen', screenId: 'abuse'}},
			{
				id: 'private_info',
				label: 'report_flow.label.private_info',
				outcome: {type: 'screen', screenId: 'private_info'},
			},
			{
				id: 'violence_misinfo',
				label: 'report_flow.label.violence_misinfo',
				outcome: {type: 'screen', screenId: 'violence_misinfo'},
			},
			{id: 'spam', label: 'report_flow.label.spam', outcome: {type: 'submit', reason: 'spam'}},
			{
				id: 'something_else',
				label: 'report_flow.label.something_else',
				outcome: {type: 'screen', screenId: 'something_else_message'},
			},
			{id: 'dislike', label: 'report_flow.label.dislike', outcome: {type: 'end'}, surface: 'in_app'},
			{id: 'dsa', label: 'report_flow.label.dsa', outcome: {type: 'link', link: 'dsa'}, surface: 'in_app'},
		],
	},
	{
		id: 'abuse',
		title: 'report_flow.screen.abuse.title',
		options: [
			{id: 'minor_risk', label: 'report_flow.label.minor_risk', outcome: {type: 'screen', screenId: 'minor_risk'}},
			{id: 'threat', label: 'report_flow.label.threat', outcome: {type: 'screen', screenId: 'threat'}},
			{id: 'harassment', label: 'report_flow.label.harassment', outcome: {type: 'screen', screenId: 'harassment'}},
			{id: 'hate', label: 'report_flow.label.hate', outcome: {type: 'screen', screenId: 'hate'}},
			{id: 'sexual', label: 'report_flow.label.sexual', outcome: {type: 'screen', screenId: 'sexual'}},
			{id: 'rude_language', label: 'report_flow.label.rude_language', outcome: {type: 'end'}, surface: 'in_app'},
		],
	},
	{
		id: 'harassment',
		title: 'report_flow.screen.harassment.title',
		options: [
			{
				id: 'harassment_direct',
				label: 'report_flow.label.harassment_direct',
				outcome: {type: 'submit', reason: 'harassment'},
			},
			{
				id: 'unwanted_contact',
				label: 'report_flow.label.unwanted_contact',
				outcome: {type: 'submit', reason: 'unwanted_contact'},
			},
			{id: 'raid', label: 'report_flow.label.raid', outcome: {type: 'submit', reason: 'raid'}},
			{
				id: 'false_claims',
				label: 'report_flow.label.false_claims',
				outcome: {type: 'submit', reason: 'false_claims_about_person'},
			},
		],
	},
	{
		id: 'hate',
		title: 'report_flow.screen.hate.title',
		options: [
			{id: 'hate_slurs', label: 'report_flow.label.hate_slurs', outcome: {type: 'submit', reason: 'hate_slurs'}},
			{
				id: 'hate_dehumanizing',
				label: 'report_flow.label.hate_dehumanizing',
				outcome: {type: 'submit', reason: 'hate_dehumanizing'},
			},
			{
				id: 'hate_incitement',
				label: 'report_flow.label.hate_incitement',
				outcome: {type: 'submit', reason: 'hate_incitement'},
			},
			{
				id: 'atrocity_denial',
				label: 'report_flow.label.atrocity_denial',
				outcome: {type: 'submit', reason: 'atrocity_denial'},
			},
			{
				id: 'lgbtq_targeting',
				label: 'report_flow.label.lgbtq_targeting',
				outcome: {type: 'submit', reason: 'lgbtq_targeting'},
			},
			{
				id: 'conversion_therapy',
				label: 'report_flow.label.conversion_therapy',
				outcome: {type: 'submit', reason: 'conversion_therapy'},
			},
			{id: 'hate_other', label: 'report_flow.label.hate_other', outcome: {type: 'submit', reason: 'hate_other'}},
		],
	},
	{
		id: 'sexual',
		title: 'report_flow.screen.sexual.title',
		subtitle: 'report_flow.screen.sexual.subtitle',
		urgent: true,
		options: [
			{
				id: 'minor_sexual',
				label: 'report_flow.label.minor_sexual',
				outcome: {type: 'screen', screenId: 'minor_sexual'},
			},
			{
				id: 'sexual_unwanted',
				label: 'report_flow.label.sexual_unwanted',
				outcome: {type: 'submit', reason: 'sexual_unwanted'},
			},
			{
				id: 'adult_content_unmarked',
				label: 'report_flow.label.adult_content_unmarked',
				outcome: {type: 'submit', reason: 'adult_content_unmarked'},
			},
			{
				id: 'intimate_image_abuse',
				label: 'report_flow.label.intimate_image_abuse',
				outcome: {type: 'submit', reason: 'intimate_image_abuse'},
			},
			{
				id: 'sexual_exploitation',
				label: 'report_flow.label.sexual_exploitation',
				outcome: {type: 'submit', reason: 'sexual_exploitation'},
			},
		],
	},
	{
		id: 'minor_sexual',
		title: 'report_flow.screen.minor_sexual.title',
		subtitle: 'report_flow.screen.minor_sexual.subtitle',
		urgent: true,
		options: [
			{id: 'csam', label: 'report_flow.label.csam', outcome: {type: 'submit', reason: 'csam'}},
			{
				id: 'minor_sexual_contact',
				label: 'report_flow.label.minor_sexual_contact',
				outcome: {type: 'submit', reason: 'minor_sexual_contact'},
			},
			{
				id: 'minor_sexual_content',
				label: 'report_flow.label.minor_sexual_content',
				outcome: {type: 'submit', reason: 'minor_sexual_content'},
			},
			{
				id: 'minor_sexualization',
				label: 'report_flow.label.minor_sexualization',
				outcome: {type: 'submit', reason: 'minor_sexualization'},
			},
			{
				id: 'minor_fictional',
				label: 'report_flow.label.minor_fictional',
				outcome: {type: 'submit', reason: 'minor_fictional_sexualization'},
			},
		],
	},
	{
		id: 'threat',
		title: 'report_flow.screen.threat.title',
		urgent: true,
		options: [
			{
				id: 'violent_threat',
				label: 'report_flow.label.violent_threat',
				outcome: {type: 'submit', reason: 'violent_threat'},
			},
			{
				id: 'violence_incitement',
				label: 'report_flow.label.violence_incitement',
				outcome: {type: 'submit', reason: 'violence_incitement'},
			},
			{
				id: 'violence_glorification',
				label: 'report_flow.label.violence_glorification',
				outcome: {type: 'submit', reason: 'violence_glorification'},
			},
		],
	},
	{
		id: 'minor_risk',
		title: 'report_flow.screen.minor_risk.title',
		urgent: true,
		options: [
			{
				id: 'minor_grooming',
				label: 'report_flow.label.minor_grooming',
				outcome: {type: 'submit', reason: 'minor_grooming'},
			},
			{
				id: 'minor_contact',
				label: 'report_flow.label.minor_contact',
				outcome: {type: 'submit', reason: 'minor_contact'},
			},
			{
				id: 'minor_sexual',
				label: 'report_flow.label.minor_sexual',
				outcome: {type: 'screen', screenId: 'minor_sexual'},
			},
			{
				id: 'minor_in_adult_space',
				label: 'report_flow.label.minor_in_adult_space',
				outcome: {type: 'submit', reason: 'minor_in_adult_space'},
			},
		],
	},
	{
		id: 'violence_misinfo',
		title: 'report_flow.screen.violence_misinfo.title',
		options: [
			{id: 'false_info', label: 'report_flow.label.false_info', outcome: {type: 'screen', screenId: 'false_info'}},
			{
				id: 'violence_glorification',
				label: 'report_flow.label.violence_glorification',
				outcome: {type: 'submit', reason: 'violence_glorification'},
			},
			{
				id: 'graphic_violence',
				label: 'report_flow.label.graphic_violence',
				outcome: {type: 'submit', reason: 'graphic_violence'},
			},
			{id: 'terrorism', label: 'report_flow.label.terrorism', outcome: {type: 'submit', reason: 'terrorism_extremism'}},
			{id: 'hate', label: 'report_flow.label.hate', outcome: {type: 'screen', screenId: 'hate'}},
		],
	},
	{
		id: 'false_info',
		title: 'report_flow.screen.false_info.title',
		subtitle: 'report_flow.screen.false_info.subtitle',
		subtitleVariants: {selfHosted: 'report_flow.screen.false_info.subtitle_self_hosted'},
		options: [
			{
				id: 'false_claims',
				label: 'report_flow.label.false_claims',
				outcome: {type: 'submit', reason: 'false_claims_about_person'},
			},
			{
				id: 'harmful_false_claims',
				label: 'report_flow.label.harmful_false_claims',
				outcome: {type: 'submit', reason: 'harmful_false_claims'},
			},
			{
				id: 'synthetic_media',
				label: 'report_flow.label.synthetic_media',
				outcome: {type: 'submit', reason: 'deceptive_synthetic_media'},
			},
		],
	},
	{
		id: 'private_info',
		title: 'report_flow.screen.private_info.title',
		subtitle: 'report_flow.screen.private_info.subtitle',
		checklist: {items: PRIVATE_INFO_ITEMS, minChecked: 1, outcome: {type: 'submit', reason: 'doxxing'}},
	},
	{
		id: 'something_else_message',
		title: 'report_flow.screen.something_else_message.title',
		options: [
			{id: 'self_harm', label: 'report_flow.label.self_harm', outcome: {type: 'screen', screenId: 'self_harm'}},
			{
				id: 'too_young',
				label: 'report_flow.label.too_young',
				outcome: {type: 'screen', screenId: 'age_stated_message'},
			},
			{
				id: 'impersonation',
				label: 'report_flow.label.impersonation',
				outcome: {type: 'screen', screenId: 'impersonation'},
			},
			{id: 'fake_account', label: 'report_flow.label.fake_account', outcome: {type: 'submit', reason: 'fake_account'}},
			{
				id: 'malware_phishing',
				label: 'report_flow.label.malware_phishing',
				outcome: {type: 'submit', reason: 'malware_phishing'},
			},
			{
				id: 'account_trading',
				label: 'report_flow.label.account_trading',
				outcome: {type: 'submit', reason: 'account_trading'},
			},
			{
				id: 'illegal_goods',
				label: 'report_flow.label.illegal_goods',
				outcome: {type: 'submit', reason: 'illegal_goods'},
			},
			{
				id: 'illegal_activity',
				label: 'report_flow.label.illegal_activity',
				outcome: {type: 'submit', reason: 'illegal_activity'},
			},
			{
				id: 'copyright',
				label: 'report_flow.label.copyright',
				outcome: {type: 'link', link: 'copyright'},
				surface: 'in_app',
			},
			{
				id: 'copyright_notice',
				label: 'report_flow.label.copyright',
				outcome: {type: 'submit', reason: 'copyright'},
				surface: 'dsa',
			},
			{id: 'other', label: 'report_flow.label.other', outcome: {type: 'submit', reason: 'other'}},
		],
	},
	{
		id: 'age_stated_message',
		title: 'report_flow.screen.age_stated_message.title',
		options: [
			{id: 'age_yes', label: 'report_flow.label.age_yes_message', outcome: {type: 'submit', reason: 'underage'}},
			{id: 'age_no', label: 'report_flow.label.age_no_message', outcome: {type: 'end', noticeId: 'need_more_info'}},
		],
	},
	{
		id: 'self_harm',
		title: 'report_flow.screen.self_harm.title',
		subtitle: 'report_flow.screen.self_harm.subtitle',
		subtitleVariants: {dsa: null},
		urgent: true,
		options: [
			{
				id: 'worried_self_harm',
				label: 'report_flow.label.worried_self_harm',
				outcome: {type: 'submit', reason: 'wellbeing_concern'},
				surface: 'in_app',
			},
			{
				id: 'worried_suicide',
				label: 'report_flow.label.worried_suicide',
				outcome: {type: 'submit', reason: 'wellbeing_concern'},
				surface: 'in_app',
			},
			{
				id: 'self_harm_promotion',
				label: 'report_flow.label.self_harm_promotion',
				outcome: {type: 'submit', reason: 'self_harm_promotion'},
			},
			{
				id: 'suicide_promotion',
				label: 'report_flow.label.suicide_promotion',
				outcome: {type: 'submit', reason: 'suicide_promotion'},
			},
			{
				id: 'eating_disorder_promotion',
				label: 'report_flow.label.eating_disorder_promotion',
				outcome: {type: 'submit', reason: 'eating_disorder_promotion'},
			},
			{
				id: 'self_harm_graphic',
				label: 'report_flow.label.self_harm_graphic',
				outcome: {type: 'submit', reason: 'self_harm_graphic'},
			},
			{
				id: 'self_harm_pressure',
				label: 'report_flow.label.self_harm_pressure',
				outcome: {type: 'submit', reason: 'self_harm_pressure'},
			},
			{id: 'crisis_lines', label: 'report_flow.label.crisis_lines', outcome: {type: 'link', link: 'crisis_lines'}},
		],
	},
	{
		id: 'impersonation',
		title: 'report_flow.screen.impersonation.title',
		subtitle: 'report_flow.screen.impersonation.subtitle',
		options: [
			{id: 'fraud', label: 'report_flow.label.fraud', outcome: {type: 'submit', reason: 'fraud'}},
			{
				id: 'impersonation_staff',
				label: 'report_flow.label.impersonation_staff',
				outcome: {type: 'submit', reason: 'impersonation_staff'},
			},
			{
				id: 'impersonation_person',
				label: 'report_flow.label.impersonation_person',
				outcome: {type: 'submit', reason: 'impersonation_person'},
			},
			{
				id: 'impersonation_public_figure',
				label: 'report_flow.label.impersonation_public_figure',
				outcome: {type: 'submit', reason: 'impersonation_public_figure'},
			},
			{
				id: 'impersonation_organization',
				label: 'report_flow.label.impersonation_organization',
				outcome: {type: 'submit', reason: 'impersonation_organization'},
			},
		],
	},
	{
		id: 'profile_intro',
		title: 'report_flow.screen.profile_intro.title',
		subtitle: 'report_flow.screen.profile_intro.subtitle',
		optionsHeading: 'report_flow.screen.profile_intro.options_heading',
		options: [{id: 'learn_more', label: 'report_flow.label.learn_more', outcome: {type: 'link', link: 'guidelines'}}],
		nextScreenId: 'profile_parts',
	},
	{
		id: 'profile_parts',
		title: 'report_flow.screen.profile_parts.title',
		subtitle: 'report_flow.screen.profile_parts.subtitle',
		checklist: {
			items: [
				{id: 'photo', label: 'report_flow.label.photo', description: 'report_flow.description.photo'},
				{id: 'name', label: 'report_flow.label.name', description: 'report_flow.description.name'},
				{
					id: 'profile_text',
					label: 'report_flow.label.profile_text',
					description: 'report_flow.description.profile_text',
				},
			],
			minChecked: 1,
			outcome: {type: 'screen', screenId: 'root_user'},
		},
	},
	{
		id: 'root_user',
		title: 'report_flow.screen.root_user.title',
		subtitle: 'report_flow.screen.root_user.subtitle',
		options: [
			{id: 'abuse', label: 'report_flow.label.abuse', outcome: {type: 'screen', screenId: 'profile_abuse'}},
			{
				id: 'hate_violence',
				label: 'report_flow.label.hate_violence',
				outcome: {type: 'screen', screenId: 'profile_hate_violence'},
			},
			{
				id: 'impersonation',
				label: 'report_flow.label.impersonation',
				outcome: {type: 'screen', screenId: 'impersonation'},
			},
			{id: 'spam', label: 'report_flow.label.spam', outcome: {type: 'screen', screenId: 'spam_profile'}},
			{
				id: 'something_else',
				label: 'report_flow.label.something_else',
				outcome: {type: 'screen', screenId: 'something_else_user'},
			},
			{id: 'dsa', label: 'report_flow.label.dsa', outcome: {type: 'link', link: 'dsa'}, surface: 'in_app'},
		],
	},
	{
		id: 'spam_profile',
		title: 'report_flow.screen.spam_profile.title',
		options: [
			{id: 'spam_profile', label: 'report_flow.label.spam_profile', outcome: {type: 'submit', reason: 'spam'}},
			{
				id: 'fake_account',
				label: 'report_flow.label.fake_account_profile',
				outcome: {type: 'submit', reason: 'fake_account'},
			},
			{
				id: 'account_trading',
				label: 'report_flow.label.account_trading_communities',
				outcome: {type: 'submit', reason: 'account_trading'},
			},
			{
				id: 'platform_abuse',
				label: 'report_flow.label.platform_abuse',
				outcome: {type: 'submit', reason: 'platform_abuse'},
			},
		],
	},
	{
		id: 'profile_abuse',
		title: 'report_flow.screen.profile_abuse.title',
		options: [
			{
				id: 'harassment',
				label: 'report_flow.label.harassment_profile',
				outcome: {type: 'submit', reason: 'harassment'},
			},
			{id: 'hate', label: 'report_flow.label.hate', outcome: {type: 'screen', screenId: 'hate'}},
			{id: 'sexual', label: 'report_flow.label.sexual', outcome: {type: 'screen', screenId: 'profile_sexual'}},
			{id: 'threat', label: 'report_flow.label.threat', outcome: {type: 'screen', screenId: 'profile_threat'}},
			{
				id: 'minor_sexual',
				label: 'report_flow.label.minor_sexual',
				outcome: {type: 'screen', screenId: 'profile_minor_sexual'},
			},
		],
	},
	{
		id: 'profile_sexual',
		title: 'report_flow.screen.profile_sexual.title',
		subtitle: 'report_flow.screen.sexual.subtitle',
		urgent: true,
		options: [
			{
				id: 'minor_sexual',
				label: 'report_flow.label.minor_sexual',
				outcome: {type: 'screen', screenId: 'profile_minor_sexual'},
			},
			{
				id: 'adult_content_profile',
				label: 'report_flow.label.adult_content_profile',
				outcome: {type: 'submit', reason: 'adult_content_unmarked'},
			},
			{
				id: 'intimate_image_abuse',
				label: 'report_flow.label.intimate_image_abuse_profile',
				outcome: {type: 'submit', reason: 'intimate_image_abuse'},
			},
			{
				id: 'sexual_exploitation',
				label: 'report_flow.label.sexual_exploitation',
				outcome: {type: 'submit', reason: 'sexual_exploitation'},
			},
		],
	},
	{
		id: 'profile_minor_sexual',
		title: 'report_flow.screen.profile_minor_sexual.title',
		subtitle: 'report_flow.screen.minor_sexual.subtitle',
		urgent: true,
		options: [
			{id: 'csam', label: 'report_flow.label.csam', outcome: {type: 'submit', reason: 'csam'}},
			{
				id: 'minor_sexual_content',
				label: 'report_flow.label.minor_sexual_content_profile',
				outcome: {type: 'submit', reason: 'minor_sexual_content'},
			},
			{
				id: 'minor_sexualization',
				label: 'report_flow.label.minor_sexualization_profile',
				outcome: {type: 'submit', reason: 'minor_sexualization'},
			},
			{
				id: 'minor_fictional',
				label: 'report_flow.label.minor_fictional',
				outcome: {type: 'submit', reason: 'minor_fictional_sexualization'},
			},
		],
	},
	{
		id: 'profile_threat',
		title: 'report_flow.screen.profile_threat.title',
		urgent: true,
		options: [
			{
				id: 'violent_threat',
				label: 'report_flow.label.violent_threat',
				outcome: {type: 'submit', reason: 'violent_threat'},
			},
			{
				id: 'violence_incitement',
				label: 'report_flow.label.violence_incitement',
				outcome: {type: 'submit', reason: 'violence_incitement'},
			},
			{
				id: 'violence_glorification',
				label: 'report_flow.label.violence_glorification',
				outcome: {type: 'submit', reason: 'violence_glorification'},
			},
		],
	},
	{
		id: 'profile_hate_violence',
		title: 'report_flow.screen.profile_hate_violence.title',
		options: [
			{id: 'hate', label: 'report_flow.label.hate', outcome: {type: 'screen', screenId: 'hate'}},
			{
				id: 'violence_glorification',
				label: 'report_flow.label.violence_glorification',
				outcome: {type: 'submit', reason: 'violence_glorification'},
			},
			{
				id: 'graphic_violence',
				label: 'report_flow.label.graphic_violence',
				outcome: {type: 'submit', reason: 'graphic_violence'},
			},
			{id: 'terrorism', label: 'report_flow.label.terrorism', outcome: {type: 'submit', reason: 'terrorism_extremism'}},
		],
	},
	{
		id: 'something_else_user',
		title: 'report_flow.screen.something_else_user.title',
		options: [
			{
				id: 'too_young',
				label: 'report_flow.label.too_young',
				outcome: {type: 'screen', screenId: 'age_stated_profile'},
			},
			{
				id: 'private_info',
				label: 'report_flow.label.private_info_profile',
				outcome: {type: 'screen', screenId: 'profile_private_info'},
			},
			{
				id: 'self_harm',
				label: 'report_flow.label.self_harm_profile',
				outcome: {type: 'screen', screenId: 'crisis_support'},
			},
			{
				id: 'malware_phishing',
				label: 'report_flow.label.malware_phishing',
				outcome: {type: 'submit', reason: 'malware_phishing'},
			},
			{
				id: 'account_trading',
				label: 'report_flow.label.account_trading',
				outcome: {type: 'submit', reason: 'account_trading'},
			},
			{id: 'ban_evasion', label: 'report_flow.label.ban_evasion', outcome: {type: 'submit', reason: 'ban_evasion'}},
			{
				id: 'illegal_goods',
				label: 'report_flow.label.illegal_goods',
				outcome: {type: 'submit', reason: 'illegal_goods'},
			},
			{
				id: 'illegal_activity',
				label: 'report_flow.label.illegal_activity',
				outcome: {type: 'submit', reason: 'illegal_activity'},
			},
			{
				id: 'copyright',
				label: 'report_flow.label.copyright',
				outcome: {type: 'link', link: 'copyright'},
				surface: 'in_app',
			},
			{
				id: 'copyright_notice',
				label: 'report_flow.label.copyright',
				outcome: {type: 'submit', reason: 'copyright'},
				surface: 'dsa',
			},
			{id: 'other', label: 'report_flow.label.other', outcome: {type: 'submit', reason: 'other'}},
		],
	},
	{
		id: 'age_stated_profile',
		title: 'report_flow.screen.age_stated_profile.title',
		options: [
			{id: 'age_yes', label: 'report_flow.label.age_yes_profile', outcome: {type: 'submit', reason: 'underage'}},
			{
				id: 'age_no',
				label: 'report_flow.label.age_no_profile',
				outcome: {type: 'end', noticeId: 'need_more_info_profile'},
			},
		],
	},
	{
		id: 'crisis_support',
		title: 'report_flow.screen.crisis_support.title',
		subtitle: 'report_flow.screen.crisis_support.subtitle',
		urgent: true,
		options: [
			{id: 'crisis_lines', label: 'report_flow.label.crisis_lines', outcome: {type: 'link', link: 'crisis_lines'}},
		],
		nextScreenId: 'self_harm_profile',
	},
	{
		id: 'self_harm_profile',
		title: 'report_flow.screen.self_harm_profile.title',
		subtitle: 'report_flow.screen.self_harm.subtitle',
		subtitleVariants: {dsa: null},
		urgent: true,
		options: [
			{
				id: 'worried',
				label: 'report_flow.label.worried_profile',
				outcome: {type: 'submit', reason: 'wellbeing_concern'},
				surface: 'in_app',
			},
			{
				id: 'self_harm_promotion',
				label: 'report_flow.label.self_harm_promotion_profile',
				outcome: {type: 'submit', reason: 'self_harm_promotion'},
			},
			{
				id: 'suicide_promotion',
				label: 'report_flow.label.suicide_promotion_profile',
				outcome: {type: 'submit', reason: 'suicide_promotion'},
			},
			{
				id: 'eating_disorder_promotion',
				label: 'report_flow.label.eating_disorder_promotion_profile',
				outcome: {type: 'submit', reason: 'eating_disorder_promotion'},
			},
			{
				id: 'self_harm_graphic',
				label: 'report_flow.label.self_harm_graphic',
				outcome: {type: 'submit', reason: 'self_harm_graphic'},
			},
		],
	},
	{
		id: 'profile_private_info',
		title: 'report_flow.screen.profile_private_info.title',
		subtitle: 'report_flow.screen.private_info.subtitle',
		checklist: {items: PRIVATE_INFO_ITEMS, minChecked: 1, outcome: {type: 'submit', reason: 'doxxing'}},
	},
	{
		id: 'community_parts',
		title: 'report_flow.screen.community_parts.title',
		subtitle: 'report_flow.screen.community_parts.subtitle',
		checklist: {
			items: [
				{id: 'activity', label: 'report_flow.label.member_activity'},
				{id: 'name', label: 'report_flow.label.community_name'},
				{id: 'channel_names', label: 'report_flow.label.channel_names'},
				{id: 'icon', label: 'report_flow.label.community_icon'},
				{id: 'banner', label: 'report_flow.label.community_banner'},
				{id: 'emoji_stickers', label: 'report_flow.label.emoji_stickers'},
				{id: 'splash', label: 'report_flow.label.invite_splash'},
				{id: 'discovery_listing', label: 'report_flow.label.discovery_listing'},
			],
			minChecked: 1,
			outcome: {type: 'screen', screenId: 'root_guild'},
		},
	},
	{
		id: 'root_guild',
		title: 'report_flow.screen.root_guild.title',
		subtitle: 'report_flow.screen.root_guild.subtitle',
		options: [
			{
				id: 'spam',
				label: 'report_flow.label.spam_or_manipulation',
				outcome: {type: 'screen', screenId: 'spam_kind_guild'},
			},
			{id: 'abuse', label: 'report_flow.label.abuse', outcome: {type: 'screen', screenId: 'abuse_guild'}},
			{
				id: 'violence_misinfo',
				label: 'report_flow.label.violence_misinfo',
				outcome: {type: 'screen', screenId: 'violence_misinfo_guild'},
			},
			{
				id: 'something_else',
				label: 'report_flow.label.something_else',
				outcome: {type: 'screen', screenId: 'something_else_guild'},
			},
		],
	},
	{
		id: 'spam_kind_guild',
		title: 'report_flow.screen.spam_kind_guild.title',
		options: [
			{id: 'spam_messages', label: 'report_flow.label.spam_bulk', outcome: {type: 'submit', reason: 'spam'}},
			{
				id: 'metric_manipulation',
				label: 'report_flow.label.metric_manipulation',
				outcome: {type: 'submit', reason: 'metric_manipulation'},
			},
			{
				id: 'account_trading',
				label: 'report_flow.label.account_trading_communities',
				outcome: {type: 'submit', reason: 'account_trading'},
			},
			{
				id: 'platform_abuse',
				label: 'report_flow.label.platform_abuse',
				outcome: {type: 'submit', reason: 'platform_abuse'},
			},
		],
	},
	{
		id: 'abuse_guild',
		title: 'report_flow.screen.abuse_guild.title',
		options: [
			{id: 'raid', label: 'report_flow.label.raid_guild', outcome: {type: 'submit', reason: 'raid'}},
			{id: 'harassment', label: 'report_flow.label.harassment_guild', outcome: {type: 'submit', reason: 'harassment'}},
			{id: 'hate', label: 'report_flow.label.hate', outcome: {type: 'screen', screenId: 'hate'}},
			{id: 'sexual', label: 'report_flow.label.sexual', outcome: {type: 'screen', screenId: 'sexual_guild'}},
			{id: 'threat', label: 'report_flow.label.threat', outcome: {type: 'screen', screenId: 'threat'}},
			{
				id: 'minor_risk',
				label: 'report_flow.label.minor_risk_guild',
				outcome: {type: 'screen', screenId: 'minor_risk_guild'},
			},
		],
	},
	{
		id: 'sexual_guild',
		title: 'report_flow.screen.sexual_guild.title',
		subtitle: 'report_flow.screen.sexual_guild.subtitle',
		urgent: true,
		options: [
			{
				id: 'minor_sexual',
				label: 'report_flow.label.minor_sexual_guild',
				outcome: {type: 'screen', screenId: 'minor_sexual_guild'},
			},
			{
				id: 'adult_content_unmarked',
				label: 'report_flow.label.adult_content_no_gate',
				outcome: {type: 'submit', reason: 'adult_content_unmarked'},
			},
			{
				id: 'intimate_image_abuse',
				label: 'report_flow.label.intimate_image_abuse_guild',
				outcome: {type: 'submit', reason: 'intimate_image_abuse'},
			},
			{
				id: 'sexual_exploitation',
				label: 'report_flow.label.sexual_exploitation',
				outcome: {type: 'submit', reason: 'sexual_exploitation'},
			},
		],
	},
	{
		id: 'minor_sexual_guild',
		title: 'report_flow.screen.minor_sexual_guild.title',
		subtitle: 'report_flow.screen.minor_sexual.subtitle',
		urgent: true,
		options: [
			{id: 'csam', label: 'report_flow.label.csam', outcome: {type: 'submit', reason: 'csam'}},
			{
				id: 'minor_sexual_contact',
				label: 'report_flow.label.minor_sexual_contact_guild',
				outcome: {type: 'submit', reason: 'minor_sexual_contact'},
			},
			{
				id: 'minor_sexual_content',
				label: 'report_flow.label.minor_sexual_content_guild',
				outcome: {type: 'submit', reason: 'minor_sexual_content'},
			},
			{
				id: 'minor_sexualization',
				label: 'report_flow.label.minor_sexualization_guild',
				outcome: {type: 'submit', reason: 'minor_sexualization'},
			},
			{
				id: 'minor_fictional',
				label: 'report_flow.label.minor_fictional',
				outcome: {type: 'submit', reason: 'minor_fictional_sexualization'},
			},
		],
	},
	{
		id: 'minor_risk_guild',
		title: 'report_flow.screen.minor_risk_guild.title',
		urgent: true,
		options: [
			{
				id: 'minor_dating_community',
				label: 'report_flow.label.minor_dating_community',
				outcome: {type: 'submit', reason: 'minor_dating_community'},
			},
			{
				id: 'minor_grooming',
				label: 'report_flow.label.minor_grooming_guild',
				outcome: {type: 'submit', reason: 'minor_grooming'},
			},
			{
				id: 'minor_in_adult_space',
				label: 'report_flow.label.minor_in_adult_space_guild',
				outcome: {type: 'submit', reason: 'minor_in_adult_space'},
			},
			{
				id: 'minor_sexual',
				label: 'report_flow.label.minor_sexual_guild',
				outcome: {type: 'screen', screenId: 'minor_sexual_guild'},
			},
		],
	},
	{
		id: 'violence_misinfo_guild',
		title: 'report_flow.screen.violence_misinfo_guild.title',
		options: [
			{
				id: 'terrorism',
				label: 'report_flow.label.terrorism_guild',
				outcome: {type: 'submit', reason: 'terrorism_extremism'},
			},
			{
				id: 'violence_glorification',
				label: 'report_flow.label.violence_glorification',
				outcome: {type: 'submit', reason: 'violence_glorification'},
			},
			{
				id: 'graphic_violence',
				label: 'report_flow.label.graphic_violence',
				outcome: {type: 'submit', reason: 'graphic_violence'},
			},
			{id: 'false_info', label: 'report_flow.label.false_info', outcome: {type: 'screen', screenId: 'false_info'}},
			{id: 'hate', label: 'report_flow.label.hate', outcome: {type: 'screen', screenId: 'hate'}},
		],
	},
	{
		id: 'something_else_guild',
		title: 'report_flow.screen.something_else_guild.title',
		options: [
			{
				id: 'permits_violations',
				label: 'report_flow.label.permits_violations',
				outcome: {type: 'submit', reason: 'permits_violations'},
			},
			{
				id: 'self_harm',
				label: 'report_flow.label.self_harm_guild',
				outcome: {type: 'screen', screenId: 'self_harm_guild'},
			},
			{
				id: 'malware_phishing',
				label: 'report_flow.label.malware_guild',
				outcome: {type: 'submit', reason: 'malware_phishing'},
			},
			{
				id: 'illegal_goods',
				label: 'report_flow.label.illegal_goods',
				outcome: {type: 'submit', reason: 'illegal_goods'},
			},
			{
				id: 'illegal_activity',
				label: 'report_flow.label.illegal_activity',
				outcome: {type: 'submit', reason: 'illegal_activity'},
			},
			{
				id: 'impersonation',
				label: 'report_flow.label.impersonation_guild',
				outcome: {type: 'submit', reason: 'impersonation_organization'},
			},
			{id: 'fraud', label: 'report_flow.label.fraud_guild', outcome: {type: 'submit', reason: 'fraud'}},
			{id: 'copyright_notice', label: 'report_flow.label.copyright', outcome: {type: 'submit', reason: 'copyright'}},
			{id: 'other', label: 'report_flow.label.other', outcome: {type: 'submit', reason: 'other'}},
		],
	},
	{
		id: 'self_harm_guild',
		title: 'report_flow.screen.self_harm_guild.title',
		urgent: true,
		options: [
			{
				id: 'self_harm_promotion',
				label: 'report_flow.label.self_harm_promotion',
				outcome: {type: 'submit', reason: 'self_harm_promotion'},
			},
			{
				id: 'suicide_promotion',
				label: 'report_flow.label.suicide_promotion',
				outcome: {type: 'submit', reason: 'suicide_promotion'},
			},
			{
				id: 'eating_disorder_promotion',
				label: 'report_flow.label.eating_disorder_promotion',
				outcome: {type: 'submit', reason: 'eating_disorder_promotion'},
			},
			{
				id: 'self_harm_graphic',
				label: 'report_flow.label.self_harm_graphic',
				outcome: {type: 'submit', reason: 'self_harm_graphic'},
			},
			{
				id: 'self_harm_pressure',
				label: 'report_flow.label.self_harm_pressure_guild',
				outcome: {type: 'submit', reason: 'self_harm_pressure'},
			},
			{id: 'crisis_lines', label: 'report_flow.label.crisis_lines', outcome: {type: 'link', link: 'crisis_lines'}},
		],
	},
];
