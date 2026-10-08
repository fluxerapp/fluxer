// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const REPORT_SUMMARY_TITLE_DESCRIPTOR = msg({
	message: 'Check your report',
	comment: 'Report flow: title of the summary screen shown before the report is sent.',
});
export const REPORT_SUMMARY_SUBTITLE_DESCRIPTOR = msg({
	message: 'Make sure this looks right before you send it.',
	comment: 'Report flow: subtitle of the summary screen. Asks the reporter to check their answers.',
});
export const REPORT_DISCLAIMER_NO_LINK_DESCRIPTOR = msg({
	message: "Only report what you honestly believe breaks the rules, and please don't send the same report twice.",
	comment:
		'Report flow: good-faith statement on the summary screen, used when the instance has no community guidelines page.',
});
export const SELECTED_MESSAGE_DESCRIPTOR = msg({
	message: "Message you're reporting",
	comment: 'Report flow: section heading above the preview of the message being reported.',
});
export const SELECTED_USER_DESCRIPTOR = msg({
	message: "Profile you're reporting",
	comment: 'Report flow: section heading above the card of the user whose profile is being reported.',
});
export const REPORT_CATEGORY_DESCRIPTOR = msg({
	message: 'Your answers',
	comment: 'Report flow: section heading above the list of answers the reporter picked, on the summary screen.',
});
export const SUBMIT_REPORT_DESCRIPTOR = msg({
	message: 'Send report',
	comment: 'Report flow: footer button on the summary screen that sends the report to the safety team.',
});
export const BACK_DESCRIPTOR = msg({
	message: 'Back',
	comment: 'Report flow: footer button that returns to the previous question.',
});
export const DONE_DESCRIPTOR = msg({
	message: 'Done',
	comment: 'Report flow: footer button on the last screen. Closes the report window.',
});
export const THANK_YOU_TITLE_DESCRIPTOR = msg({
	message: 'Report sent',
	comment: 'Report flow: title of the final screen after a report was sent to the safety team.',
});
export const THANK_YOU_NO_REPORT_TITLE_DESCRIPTOR = msg({
	message: 'Thanks for flagging this',
	comment:
		'Report flow: title of the final screen when the chosen answer does not lead to a report, so nothing was sent. Thanks the person for raising it.',
});
export const THANK_YOU_BODY_DESCRIPTOR = msg({
	message: "The {productName} safety team will review your report. We won't reveal that it came from you.",
	comment:
		'Report flow: body of the final screen after a report was sent. {productName} is the name of the app. The second sentence promises the reported person is not told who reported them.',
});
export const THANK_YOU_NO_REPORT_BODY_DESCRIPTOR = msg({
	message:
		"Thanks for telling us. This doesn't break our rules on its own, so we didn't send a report. If it targets someone or uses slurs, report it again and choose Abusive or harmful content.",
	comment:
		'Report flow: body of the final screen when the chosen answer does not lead to a report, so nothing was sent. "Abusive or harmful content" is the name of an answer in the same flow.',
});
export const THANK_YOU_NO_REPORT_SHORT_BODY_DESCRIPTOR = msg({
	message: "Thanks for telling us. This doesn't break our rules on its own, so we didn't send a report.",
	comment:
		'Report flow: body of the final screen when the chosen answer does not lead to a report, so nothing was sent. Used when the reporter already picked a category.',
});
export const MORE_YOU_CAN_DO_DESCRIPTOR = msg({
	message: 'Your options',
	comment: 'Report flow: heading above the box of follow-up actions on the final screen.',
});
export const BLOCK_NAME_DESCRIPTOR = msg({
	message: 'Block {name}',
	comment:
		'Report flow: title of the block action on the final screen. {name} is the display name of the reported user.',
});
export const BLOCK_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Hides their messages and stops them messaging you',
	comment:
		"Report flow: short description under the block action on the final screen. Blocking hides the reported person's messages and stops them sending you direct messages.",
});
export const BLOCKED_BUTTON_DESCRIPTOR = msg({
	message: 'Blocked',
	context: 'report-flow-blocked-button',
	comment:
		'Report flow: disabled button label shown after the reporter blocked the one reported user. A state of that single person, not a list of blocked users. Use the singular or an impersonal past participle.',
});
export const URGENT_BANNER_DESCRIPTOR = msg({
	message: 'If someone is in immediate danger, contact local emergency services first.',
	comment: 'Report flow: safety banner shown on urgent questions and on the summary of an urgent report.',
});
export const LOAD_FAILED_DESCRIPTOR = msg({
	message: "Couldn't load the report form.",
	comment: 'Report flow: error shown when the report questions could not be loaded. A Try again button follows.',
});
export const FLOW_OUTDATED_DESCRIPTOR = msg({
	message: 'The report form changed. Please start again.',
	comment: 'Report flow: toast shown when the report questions changed while the reporter was answering them.',
});
export const ALREADY_REPORTED_MESSAGE_DESCRIPTOR = msg({
	message: "You've already reported this message.",
	comment: 'Report flow: note on the summary screen when the reporter already reported this message.',
});
export const ALREADY_REPORTED_PROFILE_DESCRIPTOR = msg({
	message: "You've already reported this profile today.",
	comment:
		'Report flow: note on the summary screen when the reporter already reported this user profile in the last day.',
});
export const RATE_LIMITED_DESCRIPTOR = msg({
	message: "You're sending reports too quickly. Try again later.",
	comment: 'Report flow: note on the summary screen when the reporter hit the report rate limit.',
});
export const SUBMIT_FAILED_DESCRIPTOR = msg({
	message: "Your report didn't go through. Try again.",
	comment: 'Report flow: note on the summary screen when sending the report failed for an unknown reason.',
});
export const OPENS_IN_NEW_TAB_DESCRIPTOR = msg({
	message: 'Opens in a new tab',
	comment: 'Report flow: screen reader hint on answers that open an external page.',
});
export const FINISH_ACCOUNT_SETUP_FIRST_DESCRIPTOR = msg({
	message: 'Finish account setup first',
	comment: 'Report flow: title of the notice on the summary screen when the account cannot send reports yet.',
});
export const CLAIM_AND_VERIFY_TO_REPORT_DESCRIPTOR = msg({
	message: 'Claim your account and verify your email to send reports.',
	comment: 'Report flow: body of the notice on the summary screen when the account cannot send reports yet.',
});
export const CLAIM_TO_REPORT_DESCRIPTOR = msg({
	message: 'Claim your account to send reports.',
	comment:
		'Report flow: body of the notice on the summary screen when the account cannot send reports yet, on an instance where people sign in with a username.',
});
export const FINISH_ACCOUNT_SETUP_DESCRIPTOR = msg({
	message: 'Finish account setup',
	comment: 'Report flow: button in the account notice that opens the account setup screen.',
});
