// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import type {RequiredActionFlow} from '@app/features/auth/components/modals/RequiredActionFlow';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export const ACCOUNT_VERIFICATION_REQUIRED_DESCRIPTOR = msg({
	message: 'Account verification required',
	comment: 'Required-action modal title shown when an account-level verification step is required to proceed.',
});
export const EMAIL_UPDATE_REQUIRED_DESCRIPTOR = msg({
	message: 'Email update required',
	comment: 'Required-action modal title shown when the user must update their email address to continue.',
});
export const EMAIL_RE_VERIFICATION_REQUIRED_DESCRIPTOR = msg({
	message: 'Email re-verification required',
	comment: 'Required-action modal title shown when the email address must be reverified.',
});
export const EMAIL_VERIFICATION_REQUIRED_DESCRIPTOR = msg({
	message: 'Email verification required',
	comment: 'Required-action modal title shown when the email address must be verified for the first time.',
});
export const PHONE_RE_VERIFICATION_REQUIRED_DESCRIPTOR = msg({
	message: 'Phone re-verification required',
	comment: 'Required-action modal title shown when the phone number must be reverified.',
});
export const PHONE_VERIFICATION_REQUIRED_DESCRIPTOR = msg({
	message: 'Phone verification required',
	comment: 'Required-action modal title shown when the phone number must be verified for the first time.',
});
export const EMAIL_OR_PHONE_RE_VERIFICATION_REQUIRED_DESCRIPTOR = msg({
	message: 'Email or phone re-verification required',
	comment: 'Required-action modal title shown when either email or phone needs reverification (user chooses).',
});
export const ADDITIONAL_VERIFICATION_REQUIRED_DESCRIPTOR = msg({
	message: 'Additional verification required',
	comment: 'Required-action modal title for a generic additional verification step (e.g. anti-spam).',
});
export const PLEASE_COMPLETE_THE_REQUIRED_VERIFICATION_STEPS_TO_CONTINUE_DESCRIPTOR = msg({
	message: 'Complete the required verification to continue using {productName}.',
	comment: 'Required-action modal body. Generic instructions to complete verification. Product name is interpolated.',
});
export const YOUR_CURRENT_EMAIL_ADDRESS_COULDN_T_RECEIVE_MESSAGES_DESCRIPTOR = msg({
	message: "Your email couldn't receive messages. Update it to continue using {productName}.",
	comment:
		'Required-action modal body. Explains the email needs updating because delivery failed. Product name is interpolated.',
});
export const PLEASE_REVERIFY_YOUR_EMAIL_ADDRESS_TO_CONTINUE_USING_DESCRIPTOR = msg({
	message: 'Reverify your email to continue using {productName}.',
	comment: 'Required-action modal body for email re-verification. Product name is interpolated.',
});
export const PLEASE_VERIFY_YOUR_EMAIL_ADDRESS_TO_CONTINUE_USING_DESCRIPTOR = msg({
	message: 'Verify your email to continue using {productName}.',
	comment: 'Required-action modal body for first-time email verification. Product name is interpolated.',
});
export const PLEASE_VERIFY_YOUR_PHONE_NUMBER_AGAIN_TO_CONTINUE_DESCRIPTOR = msg({
	message: 'Reverify your phone number to continue using {productName}.',
	comment: 'Required-action modal body for phone re-verification. Product name is interpolated.',
});
export const YOUR_REGISTRATION_NEEDS_AN_EXTRA_ANTI_SPAM_CHECK_DESCRIPTOR = msg({
	message: 'Your registration needs an extra anti-spam check before you can continue.',
	comment: 'Required-action modal body for an extra anti-spam verification during registration.',
});
export const PLEASE_REVERIFY_YOUR_EMAIL_ADDRESS_OR_VERIFY_YOUR_DESCRIPTOR = msg({
	message: 'Reverify your email or verify your phone to continue using {productName}.',
	comment: 'Required-action modal body when user must reverify email or verify phone. Product name is interpolated.',
});
export const PLEASE_VERIFY_YOUR_EMAIL_ADDRESS_OR_PHONE_NUMBER_DESCRIPTOR = msg({
	message: 'Verify your email or phone to continue using {productName}.',
	comment:
		'Required-action modal body when the user must verify email or phone for the first time. Preserve {productName}; it is inserted by code and must appear verbatim in the translation.',
});
export const COMPLETE_ONE_OF_THE_VERIFICATION_PATHS_BELOW_TO_DESCRIPTOR = msg({
	message: 'Complete one of the verification paths below to continue using {productName}.',
	comment:
		'Required-action modal body when either of two verification paths is acceptable. Product name is interpolated.',
});
export const COMPLETE_THE_REQUIRED_EMAIL_AND_PHONE_VERIFICATION_STEPS_DESCRIPTOR = msg({
	message: 'Complete the required email and phone verification steps below to continue using {productName}.',
	comment:
		'Required-action modal body when both email and phone verification are required. Product name is interpolated.',
});
export const NEW_EMAIL_DESCRIPTOR = msg({
	message: 'New email',
	comment: 'Form field label for the new email address input in the email update flow.',
});
export const YOUR_NEW_EMAIL_DESCRIPTOR = msg({
	message: 'your new email',
	comment: 'Placeholder text inside the new email field. Sentence-fragment hint, intentionally lowercase.',
});
export const COUNTRY_DESCRIPTOR = msg({
	message: 'Country',
	comment: 'Form field label for the country selector in the phone verification step.',
});
export const SEARCH_COUNTRIES_DESCRIPTOR = msg({
	message: 'Search countries…',
	comment: 'Placeholder text inside the country search input.',
});
export const PHONE_NUMBER_DESCRIPTOR = msg({
	message: 'Phone number',
	comment: 'Form field label for the phone number input.',
});
export const PHONE_DESCRIPTOR = msg({
	message: 'Phone',
	comment: 'Tab label for the phone verification path in the required-action modal.',
});
export const PLEASE_ENTER_A_VALID_PHONE_NUMBER_DESCRIPTOR = msg({
	message: 'Enter a valid phone number',
	comment: 'Form validation error shown when the entered phone number cannot be parsed.',
});
export const PLEASE_REQUEST_A_VERIFICATION_CODE_FIRST_DESCRIPTOR = msg({
	message: 'Request a verification code first.',
	comment: 'Form validation error shown when the user tries to submit a code before requesting one.',
});
export const PHONE_NUMBER_IS_REQUIRED_DESCRIPTOR = msg({
	message: 'Phone number is required',
	comment: 'Form validation error shown when the phone number field is empty.',
});
export const VERIFICATION_CODE_SENT_CHECK_YOUR_NEW_EMAIL_INBOX_DESCRIPTOR = msg({
	message: 'Verification code sent. Check your new email inbox.',
	comment: 'Toast confirmation after sending a verification code to the new email address during email update.',
});
export const YOUR_EMAIL_ADDRESS_HAS_BEEN_UPDATED_DESCRIPTOR = msg({
	message: 'Your email address has been updated.',
	comment: 'Toast confirmation after the email address has been successfully updated.',
});
export const YOU_NEED_ACCESS_TO_YOUR_CURRENT_EMAIL_TO_DESCRIPTOR = msg({
	message:
		'You need access to your current email to change it from here. Use phone verification if available or contact support.',
	comment:
		'Body text shown when the user lacks access to their old email and must use phone verification or contact support. Keep plain and calm.',
});
export const VERIFICATION_EMAIL_SENT_PLEASE_CHECK_YOUR_INBOX_DESCRIPTOR = msg({
	message: 'Verification email sent. Check your inbox.',
	comment: 'Toast confirmation after a verification email has been sent to the existing email address.',
});
export const TOO_MANY_REQUESTS_PLEASE_TRY_AGAIN_LATER_DESCRIPTOR = msg({
	message: 'Too many requests. Try again later.',
	comment: 'Toast error shown when the verification request is rate-limited.',
});
export const FAILED_TO_SEND_VERIFICATION_EMAIL_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Failed to send verification email. Try again later.',
	comment: 'Toast error shown when sending the verification email fails.',
});
export const A_NEW_VERIFICATION_CODE_HAS_BEEN_SENT_DESCRIPTOR = msg({
	message: 'A new verification code has been sent.',
	comment: 'Toast confirmation shown after resending a verification code.',
});
export const FAILED_TO_RESEND_VERIFICATION_CODE_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Failed to resend verification code. Try again later.',
	comment: 'Toast error shown when resending the verification code fails.',
});
export const NEXT_DESCRIPTOR = msg({
	message: 'Next',
	comment: 'Primary button in the required-action modal. Moves to the next carousel step.',
});
export const START_DESCRIPTOR = msg({
	message: 'Start',
	comment: 'Primary button in the required-action modal. Starts the selected verification path.',
});
export const CONTINUE_DESCRIPTOR = msg({
	message: 'Continue',
	comment: 'Primary button in the required-action modal. Continues the current verification path.',
});
export const I_NEED_ANOTHER_WAY_DESCRIPTOR = msg({
	message: 'I need another way',
	comment: 'Secondary button in the required-action modal. Opens recovery options for the current verification step.',
});
export const USE_EMAIL_DESCRIPTOR = msg({
	message: 'Use email',
	comment: 'Button in the required-action modal. Selects the email verification path.',
});
export const USE_PHONE_DESCRIPTOR = msg({
	message: 'Use phone',
	comment: 'Button in the required-action modal. Selects the phone verification path.',
});
export const VERIFY_PHONE_DESCRIPTOR = msg({
	message: 'Verify phone',
	comment: 'Primary button in the required-action modal. Sends or submits phone verification.',
});
export const USE_DIFFERENT_EMAIL_DESCRIPTOR = msg({
	message: 'Use different email',
	comment: 'Button in the required-action modal. Starts the flow for changing to another email address.',
});
export const UPDATE_EMAIL_DESCRIPTOR = msg({
	message: 'Update email',
	comment: 'Primary button in the required-action modal. Submits a code to update the account email.',
});
export const SEND_CODE_DESCRIPTOR = msg({
	message: 'Send code',
	comment: 'Primary button in the required-action modal. Requests a verification code.',
});
export const RESEND_CODE_DESCRIPTOR = msg({
	message: 'Resend code',
	comment: 'Button in the required-action modal. Requests another verification code.',
});
export const RESEND_EMAIL_DESCRIPTOR = msg({
	message: 'Resend email',
	comment: 'Button in the required-action modal. Sends another verification email.',
});
export const GET_NEW_CODE_DESCRIPTOR = msg({
	message: 'Get new code',
	comment: 'Button in the required-action modal. Requests a new inbound SMS challenge code.',
});
export const STEP_INTRO_TITLE_DESCRIPTOR = msg({
	message: 'One quick check',
	comment: 'Required-action modal carousel step title. Introduces why account verification is required.',
});
export const STEP_INTRO_GENERIC_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'We need one verification step before you continue. This helps protect accounts and keep spam off {productName}.',
	comment:
		'Required-action modal intro text for a generic verification requirement. productName is inserted by code. Keep it calm and concise.',
});
export const STEP_INTRO_EMAIL_AND_PHONE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'We need two verification steps before you continue. We will show one step at a time.',
	comment: 'Required-action modal intro text when both email and phone verification are required.',
});
export const STEP_INTRO_EMAIL_OR_PHONE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Choose one verification method. Either method can clear this check.',
	comment: 'Required-action modal intro text when email or phone verification can satisfy the requirement.',
});
export const STEP_INTRO_BOUNCED_EMAIL_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Your email could not receive messages. Add a working email so we can keep your account reachable.',
	comment: 'Required-action modal intro text when the current account email bounced and must be replaced.',
});
export const STEP_INTRO_PHONE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'This is an anti-spam check. Your phone number is not linked to your account.',
	comment: 'Required-action modal intro text when phone verification is required.',
});
export const CHOOSE_METHOD_TITLE_DESCRIPTOR = msg({
	message: 'Choose a method',
	comment: 'Required-action modal carousel step title for choosing email or phone verification.',
});
export const CHOOSE_METHOD_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Pick the method you can complete now.',
	comment: 'Required-action modal body for choosing a verification method.',
});
export const CHECK_YOUR_EMAIL_TITLE_DESCRIPTOR = msg({
	message: 'Check your email',
	comment: 'Required-action modal carousel step title for existing-email verification.',
});
export const CHECK_YOUR_EMAIL_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Open the verification email and use the link inside. This window updates after the link opens.',
	comment: 'Required-action modal body explaining how to finish email verification.',
});
export const EMAIL_ADDRESS_LABEL_DESCRIPTOR = msg({
	message: 'Email address',
	comment: 'Label for the current account email shown in the required-action modal.',
});
export const EMAIL_HELP_TITLE_DESCRIPTOR = msg({
	message: "Can't access that email?",
	comment: 'Required-action modal carousel step title for email recovery options.',
});
export const EMAIL_HELP_SELF_SERVE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Use a different email address and verify it with a code.',
	comment: 'Required-action modal body for self-serve email recovery.',
});
export const EMAIL_HELP_SUPPORT_DESCRIPTION_DESCRIPTOR = msg({
	message: 'If you cannot use any verification method, contact support for help.',
	comment: 'Required-action modal body when the user must contact support for account recovery.',
});
export const SUPPORT_LINK_LABEL_DESCRIPTOR = msg({
	message: 'Contact support',
	comment: 'Link label in the required-action modal. Opens an email to Fluxer support.',
});
export const ALT_ROUTES_TITLE_DESCRIPTOR = msg({
	message: "If you can't verify by SMS",
	comment:
		'Heading of the alternative-routes block in the required-action modal. Introduces human review and the option to set the phone check aside. Must read as a real supported route, not fine print.',
});
export const ALT_ROUTES_HUMAN_REVIEW_DESCRIPTOR = msg({
	message:
		'Email us and a person will review your account. Tell us you cannot verify by SMS and we will take it from there. Replies come by email, so this is not instant.',
	comment:
		'Body of the human-review option in the required-action modal, rendered directly above the Contact support link. Sets the expectation that a reply takes time.',
});
export const ESCAPE_BUTTON_DESCRIPTOR = msg({
	message: 'Set this check aside',
	comment:
		'Secondary button in the required-action modal that restores the deferred phone check instead of verifying a number. Must not read as skipping or circumventing, and must not use back or undo wording, which collides with the modal back button.',
});
export const ESCAPE_HINT_NO_GUILDS_DESCRIPTOR = msg({
	message:
		'Your account works normally again straight away and nothing is removed. We can ask for this check again later.',
	comment:
		'Hint under the set-aside button in the required-action modal when the user is in no community that triggers the check. This is the common case. Do not phrase it as undoing a join.',
});
export const ESCAPE_HINT_WITH_GUILDS_DESCRIPTOR = msg({
	message:
		'You will leave {count, plural, one {# community} other {# communities}} that this check applies to, and your account works normally again.',
	comment:
		'Hint under the set-aside button in the required-action modal when the user is in communities that trigger the check. count is inserted by code.',
});
export const ESCAPE_CONFIRM_TITLE_NO_GUILDS_DESCRIPTOR = msg({
	message: 'Set this check aside?',
	comment: 'Confirmation dialog title in the required-action modal when nothing will be left.',
});
export const ESCAPE_CONFIRM_TITLE_WITH_GUILDS_DESCRIPTOR = msg({
	message: 'Leave {count, plural, one {# community} other {# communities}} and set this check aside?',
	comment:
		'Confirmation dialog title in the required-action modal when communities will be left. count is inserted by code.',
});
export const ESCAPE_CONFIRM_OUTCOME_DESCRIPTOR = msg({
	message:
		'Your account goes back to normal right away and you will not need a phone number now. This is not a permanent exemption. Joining a large or public community can bring this check back.',
	comment:
		'First line of the set-aside confirmation in the required-action modal. Always shown. Says can rather than will, because whether the check returns depends on account age and instance settings the user cannot see.',
});
export const ESCAPE_CONFIRM_LEAVE_DESCRIPTOR = msg({
	message: 'You will leave {guildNames}. Nothing you posted is deleted, and you can join again later.',
	comment:
		'Confirmation line in the required-action modal, shown only when the user is in communities that trigger the check. guildNames is a comma-separated list inserted by code. Both reassurances answer fears people actually have.',
});
export const ESCAPE_CONFIRM_OWNED_DESCRIPTOR = msg({
	message: 'You stay in {ownedNames}. Owners cannot leave their own community.',
	comment:
		'Confirmation line in the required-action modal explaining that communities the user owns are kept rather than left. ownedNames is a comma-separated list inserted by code.',
});
export const ESCAPE_CONFIRM_EMAIL_REMAINS_DESCRIPTOR = msg({
	message: 'You will still need to finish the email step.',
	comment:
		'Confirmation line in the required-action modal when an email requirement remains alongside the phone check, so the modal stays open afterwards.',
});
export const ESCAPE_CONFIRM_SUPPORT_DESCRIPTOR = msg({
	message: 'You can still email {supportEmail} for a human review at any time.',
	comment:
		'Last line of the set-aside confirmation in the required-action modal. supportEmail is inserted by code. The self-serve route never closes the support route.',
});
export const ESCAPE_CONFIRM_PRIMARY_NO_GUILDS_DESCRIPTOR = msg({
	message: 'Set this check aside',
	comment:
		'Primary button of the set-aside confirmation in the required-action modal when nothing will be left. Uses the normal button style, not the destructive one.',
});
export const ESCAPE_CONFIRM_PRIMARY_WITH_GUILDS_DESCRIPTOR = msg({
	message: 'Leave and set aside',
	comment:
		'Primary button of the set-aside confirmation in the required-action modal when communities will be left. Uses the destructive button style.',
});
export const ESCAPE_SUCCESS_TOAST_DESCRIPTOR = msg({
	message: 'Your account is back to normal.',
	comment:
		'Success toast after the phone check is set aside and no verification steps remain. Deliberately count-free, because any communities that were left already disappear from the sidebar.',
});
export const ESCAPE_SUCCESS_EMAIL_REMAINS_TOAST_DESCRIPTOR = msg({
	message: 'The phone check is set aside. One email step is left.',
	comment:
		'Success toast after the phone check is set aside while an email requirement remains, so the modal stays open on the email step. Never tell someone they are done while a modal is still in front of them.',
});
export const ESCAPE_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'This is not available on your account right now. Email {supportEmail} and a person will review it.',
	comment:
		'Shown in the required-action modal when setting the check aside is refused, and when it reported success but the phone requirement is still in force. supportEmail is inserted by code, and the address is named in full because the support link may have scrolled out of view.',
});
export const ESCAPE_FAILED_NOTHING_CHANGED_DESCRIPTOR = msg({
	message: 'That did not go through. Nothing changed. Try again.',
	comment:
		'Shown in the required-action modal when setting the check aside failed and the user was in no community that would have been left, so nothing can have been lost.',
});
export const ESCAPE_FAILED_PARTIAL_DESCRIPTOR = msg({
	message:
		'That did not finish. Some communities may already have been left, and your account is still locked. Press Set this check aside again to finish.',
	comment:
		'Shown in the required-action modal when setting the check aside failed part way through leaving communities. Retrying is safe and continues from where it stopped. Names the button so the reader knows what to press.',
});
export const ADD_WORKING_EMAIL_TITLE_DESCRIPTOR = msg({
	message: 'Add a working email',
	comment: 'Required-action modal carousel step title for replacing a bounced email address.',
});
export const ADD_WORKING_EMAIL_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Enter an email you can open. We will send a code there.',
	comment: 'Required-action modal body for entering a replacement email address.',
});
export const ENTER_NEW_EMAIL_TITLE_DESCRIPTOR = msg({
	message: 'Enter new email',
	comment: 'Required-action modal carousel step title for changing to a new email address.',
});
export const ENTER_NEW_EMAIL_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Use an email you can open. We will send a code there.',
	comment: 'Required-action modal body for entering a new email address.',
});
export const ENTER_EMAIL_CODE_TITLE_DESCRIPTOR = msg({
	message: 'Enter the email code',
	comment: 'Required-action modal carousel step title for entering an email verification code.',
});
export const ENTER_EMAIL_CODE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Enter the code sent to {emailAddress}.',
	comment:
		'Required-action modal body for entering a code sent to a new email address. emailAddress is inserted by code.',
});
export const PHONE_NUMBER_TITLE_DESCRIPTOR = msg({
	message: 'Enter your phone number',
	comment: 'Required-action modal carousel step title for entering a phone number.',
});
export const PHONE_NUMBER_DESCRIPTION_DESCRIPTOR = msg({
	message: 'We will send an SMS code when available.',
	comment: 'Required-action modal body for entering a phone number.',
});
export const PHONE_PRIVACY_DESCRIPTOR = msg({
	message:
		'Your number is not linked to your account. We keep only an encrypted marker, with no user ID, for at most {limit} verifications in about {duration} days.',
	comment: 'Required-action modal privacy note for phone verification. limit and duration are inserted by code.',
});
export const ENTER_PHONE_CODE_TITLE_DESCRIPTOR = msg({
	message: 'Enter the SMS code',
	comment: 'Required-action modal carousel step title for entering a phone SMS verification code.',
});
export const ENTER_PHONE_CODE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Enter the {digitCount}-digit code sent to {phoneNumber}.',
	comment: 'Required-action modal body for entering an SMS code. digitCount and phoneNumber are inserted by code.',
});
export const INBOUND_PHONE_START_TITLE_DESCRIPTOR = msg({
	message: 'Text us from your phone',
	comment: 'Required-action modal carousel step title for starting inbound phone verification.',
});
export const INBOUND_PHONE_START_DESCRIPTION_DESCRIPTOR = msg({
	message: 'For this check, you send us a text message instead of receiving one from us.',
	comment: 'Required-action modal body for starting an inbound phone verification challenge.',
});
export const INBOUND_PHONE_EXPENSIVE_REASON_DESCRIPTOR = msg({
	message: 'Sending an SMS to this number is expensive, so we need you to text us instead.',
	comment: 'Required-action modal explanation for an inbound phone verification challenge due to SMS cost.',
});
export const INBOUND_PHONE_DEFAULT_REASON_DESCRIPTOR = msg({
	message: 'This number needs text-in verification instead of an SMS from us.',
	comment: 'Required-action modal explanation for an inbound phone verification challenge.',
});
export const INBOUND_PHONE_PREPARE_TITLE_DESCRIPTOR = msg({
	message: 'Open your messages',
	comment: 'Required-action modal carousel step title for the first inbound phone verification instruction.',
});
export const INBOUND_PHONE_PREPARE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Open your phone messaging app and start a new text message.',
	comment: 'Required-action modal body for the first inbound phone verification instruction.',
});
export const INBOUND_PHONE_SEND_TITLE_DESCRIPTOR = msg({
	message: 'Send this code',
	comment: 'Required-action modal carousel step title for the second inbound phone verification instruction.',
});
export const INBOUND_PHONE_SEND_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Send the code to the phone number below.',
	comment: 'Required-action modal body for the second inbound phone verification instruction.',
});
export const INBOUND_PHONE_CODE_LABEL_DESCRIPTOR = msg({
	message: 'Code',
	comment: 'Label for an inbound phone verification code shown in the required-action modal.',
});
export const INBOUND_PHONE_DESTINATION_LABEL_DESCRIPTOR = msg({
	message: 'Send to',
	comment: 'Label for the destination phone number in an inbound phone verification challenge.',
});
export const INBOUND_PHONE_WAIT_TITLE_DESCRIPTOR = msg({
	message: 'Wait here',
	comment: 'Required-action modal carousel step title after the user sends an inbound phone verification text.',
});
export const INBOUND_PHONE_WAIT_DESCRIPTION_DESCRIPTOR = msg({
	message: 'This window updates automatically after we receive your message.',
	comment: 'Required-action modal body after the user sends an inbound phone verification text.',
});
export const MESSAGE_RATES_NOTICE_DESCRIPTOR = msg({
	message: 'Standard messaging rates from your carrier may apply.',
	comment: 'Required-action modal notice for inbound phone verification.',
});
export const PHONE_VERIFIED_DESCRIPTOR = msg({
	message: 'Phone number verified',
	comment: 'Toast shown after phone verification succeeds.',
});
export const CODE_DID_NOT_WORK_DESCRIPTOR = msg({
	message: "That code didn't work. Check it and try again.",
	comment: 'Required-action form error for an invalid verification code.',
});
export const CODE_EXPIRED_DESCRIPTOR = msg({
	message: 'That code expired. Request a new code.',
	comment: 'Required-action form error for an expired verification code.',
});
export const CODE_NOT_REQUESTED_DESCRIPTOR = msg({
	message: 'Request a new code first.',
	comment: 'Required-action form error when the user submits before a code exists.',
});
export const VERIFICATION_SESSION_EXPIRED_DESCRIPTOR = msg({
	message: 'This verification session expired. Start again.',
	comment: 'Required-action form error when an email-change ticket or proof is invalid or expired.',
});
export const TOO_MANY_ATTEMPTS_DESCRIPTOR = msg({
	message: 'Too many attempts. Wait a bit, then try again.',
	comment: 'Required-action error shown for rate limits.',
});
export const ENTER_VALID_EMAIL_DESCRIPTOR = msg({
	message: 'Enter a valid email address.',
	comment: 'Required-action form error for an invalid email address.',
});
export const EMAIL_ALREADY_IN_USE_DESCRIPTOR = msg({
	message: 'That email is already in use.',
	comment: 'Required-action form error when a new email is already used by another account.',
});
export const EMAIL_MUST_BE_DIFFERENT_DESCRIPTOR = msg({
	message: 'Use an email that is different from your current one.',
	comment: 'Required-action form error when the new email matches the current account email.',
});
export const EMAIL_CHANGE_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'Email update is unavailable right now. Try again later or contact support.',
	comment: 'Required-action error shown when email update cannot continue.',
});
export const ENTER_VALID_PHONE_DESCRIPTOR = msg({
	message: 'Enter a valid mobile phone number.',
	comment: 'Required-action form error for an invalid phone number.',
});
export const PHONE_CANNOT_BE_USED_DESCRIPTOR = msg({
	message: 'This phone number cannot be used. Try another mobile number or contact support.',
	comment: 'Required-action form error when the backend rejects a phone number.',
});
export const PHONE_ALREADY_USED_DESCRIPTOR = msg({
	message: 'This phone number has already been used. Try another number or contact support.',
	comment: 'Required-action form error when the backend rejects a reused phone number.',
});
export const SMS_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'SMS verification is unavailable right now. Try again later or contact support.',
	comment: 'Required-action error when the SMS provider cannot send or verify a code.',
});
export const PHONE_NOT_ELIGIBLE_DESCRIPTOR = msg({
	message: 'Phone verification is not available for this account. Use another method or contact support.',
	comment: 'Required-action error when the account is not eligible to add phone verification.',
});
export const CAPTCHA_REQUIRED_DESCRIPTOR = msg({
	message: 'A browser check is required before phone verification. Try again from the sign-in page or contact support.',
	comment: 'Required-action error when the API requires a captcha for phone verification.',
});
export const SOMETHING_WENT_WRONG_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Something went wrong. Try again.',
	comment: 'Generic required-action error fallback.',
});
export const getTitleDescriptor = (flow: RequiredActionFlow | null, emailBounced = false): MessageDescriptor => {
	if (!flow) {
		return ACCOUNT_VERIFICATION_REQUIRED_DESCRIPTOR;
	}
	switch (flow.mode) {
		case 'email':
			if (emailBounced) {
				return EMAIL_UPDATE_REQUIRED_DESCRIPTOR;
			}
			return flow.email?.reverify ? EMAIL_RE_VERIFICATION_REQUIRED_DESCRIPTOR : EMAIL_VERIFICATION_REQUIRED_DESCRIPTOR;
		case 'phone':
			return flow.phone?.reverify ? PHONE_RE_VERIFICATION_REQUIRED_DESCRIPTOR : PHONE_VERIFICATION_REQUIRED_DESCRIPTOR;
		case 'email_or_phone':
			if (flow.email?.reverify === flow.phone?.reverify) {
				return flow.reverify
					? EMAIL_OR_PHONE_RE_VERIFICATION_REQUIRED_DESCRIPTOR
					: ACCOUNT_VERIFICATION_REQUIRED_DESCRIPTOR;
			}
			return ADDITIONAL_VERIFICATION_REQUIRED_DESCRIPTOR;
		case 'email_and_phone':
			return ADDITIONAL_VERIFICATION_REQUIRED_DESCRIPTOR;
	}
};
export const getDescriptionDescriptor = (flow: RequiredActionFlow | null, emailBounced = false): MessageDescriptor => {
	if (!flow) {
		return {
			...PLEASE_COMPLETE_THE_REQUIRED_VERIFICATION_STEPS_TO_CONTINUE_DESCRIPTOR,
			values: {productName: PRODUCT_NAME},
		};
	}
	switch (flow.mode) {
		case 'email':
			if (emailBounced) {
				return {
					...YOUR_CURRENT_EMAIL_ADDRESS_COULDN_T_RECEIVE_MESSAGES_DESCRIPTOR,
					values: {productName: PRODUCT_NAME},
				};
			}
			return flow.email?.reverify
				? {...PLEASE_REVERIFY_YOUR_EMAIL_ADDRESS_TO_CONTINUE_USING_DESCRIPTOR, values: {productName: PRODUCT_NAME}}
				: {...PLEASE_VERIFY_YOUR_EMAIL_ADDRESS_TO_CONTINUE_USING_DESCRIPTOR, values: {productName: PRODUCT_NAME}};
		case 'phone':
			return flow.phone?.reverify
				? {...PLEASE_VERIFY_YOUR_PHONE_NUMBER_AGAIN_TO_CONTINUE_DESCRIPTOR, values: {productName: PRODUCT_NAME}}
				: YOUR_REGISTRATION_NEEDS_AN_EXTRA_ANTI_SPAM_CHECK_DESCRIPTOR;
		case 'email_or_phone':
			if (flow.email?.reverify === flow.phone?.reverify) {
				return flow.reverify
					? {...PLEASE_REVERIFY_YOUR_EMAIL_ADDRESS_OR_VERIFY_YOUR_DESCRIPTOR, values: {productName: PRODUCT_NAME}}
					: {...PLEASE_VERIFY_YOUR_EMAIL_ADDRESS_OR_PHONE_NUMBER_DESCRIPTOR, values: {productName: PRODUCT_NAME}};
			}
			return {...COMPLETE_ONE_OF_THE_VERIFICATION_PATHS_BELOW_TO_DESCRIPTOR, values: {productName: PRODUCT_NAME}};
		case 'email_and_phone':
			return {
				...COMPLETE_THE_REQUIRED_EMAIL_AND_PHONE_VERIFICATION_STEPS_DESCRIPTOR,
				values: {productName: PRODUCT_NAME},
			};
	}
};
