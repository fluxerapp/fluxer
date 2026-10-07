// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const MONTHLY_PRICE_DESCRIPTOR = msg({
	message: '{monthlyPrice}/mo',
	comment: 'Compact monthly price on the Plutonium page hero. Preserve the monthlyPrice placeholder exactly.',
});
export const YEARLY_PRICE_DESCRIPTOR = msg({
	message: '{yearlyPrice}/yr',
	comment: 'Compact yearly price on the Plutonium page hero. Preserve the yearlyPrice placeholder exactly.',
});
export const PRICE_OR_DESCRIPTOR = msg({
	message: 'or',
	comment: 'Compact conjunction between the monthly and yearly price on the Plutonium page hero.',
});
export const DONATION_HINT_DESCRIPTOR = msg({
	message: "Just want to support {productName}'s open source development? {donateLink}.",
	comment:
		'Hint under the Plutonium page hero buttons. productName is the app name. donateLink is a link whose text is "Donate instead".',
});
export const DONATE_INSTEAD_DESCRIPTOR = msg({
	message: 'Donate instead',
	comment: 'Link text in the Plutonium page donation hint. Opens the donation page on the website.',
});
export const SUBSCRIBE_YEARLY_DESCRIPTOR = msg({
	message: 'Get yearly',
	comment: 'Primary Plutonium page button that starts a yearly subscription checkout.',
});
export const SUBSCRIBE_MONTHLY_DESCRIPTOR = msg({
	message: 'Get monthly',
	comment: 'Secondary Plutonium page button that starts a monthly subscription checkout.',
});
export const SAVE_PERCENT_DESCRIPTOR = msg({
	message: 'Save {percent}%',
	comment: 'Badge on the yearly Plutonium button. percent is a whole number such as 17.',
});
export const GIFT_PLUTONIUM_DESCRIPTOR = msg({
	message: 'Gift {premiumProductName}',
	comment: 'Plutonium page button that opens the gift purchase dialog. premiumProductName is the paid tier name.',
});
export const REDEEM_GIFT_CODE_DESCRIPTOR = msg({
	message: 'Redeem a gift code',
	comment: 'Plutonium page link that opens the gift code redemption settings.',
});
export const MANAGE_SUBSCRIPTION_DESCRIPTOR = msg({
	message: 'Manage subscription',
	comment: 'Plutonium page hero button that scrolls to the subscription management panel.',
});
export const MEMBER_THANKS_DESCRIPTOR = msg({
	message: "You're a {premiumProductName} member. Thanks for supporting an independent communication platform.",
	comment: 'Plutonium page hero subtitle for members. premiumProductName is the paid tier name.',
});
export const VISIONARY_THANKS_DESCRIPTOR = msg({
	message: "You're a Visionary. Your perks never expire, and gifts for friends are always available.",
	comment: 'Plutonium page hero subtitle for lifetime Visionary members.',
});
export const GIFTED_THANKS_DESCRIPTOR = msg({
	message: 'Someone gifted you {premiumProductName}. Your perks stay active until {date}.',
	comment: 'Plutonium page hero subtitle for gifted members. date is a formatted calendar date.',
});
export const LAPSED_DESCRIPTOR = msg({
	message: 'Your {premiumProductName} perks have ended. Subscribe again to bring them back.',
	comment: 'Plutonium page hero subtitle when a subscription has ended. premiumProductName is the paid tier name.',
});
export const GRACE_DESCRIPTOR = msg({
	message: 'We could not renew your subscription. Update your payment method to keep your perks.',
	comment: 'Plutonium page hero subtitle while a failed renewal is in its grace period.',
});
export const GIFT_GRACE_DESCRIPTOR = msg({
	message: 'Your gift time has ended. Subscribe to keep {premiumProductName}.',
	comment:
		'Plutonium page hero subtitle while gifted premium time is in its short grace period after ending. premiumProductName is the paid tier name.',
});
export const REDEEM_ONLY_DESCRIPTOR = msg({
	message: 'Have a gift code? Redeem it to unlock {premiumProductName}.',
	comment:
		'Plutonium page hero text on an instance where the paid tier cannot be bought, only redeemed. premiumProductName is the paid tier name.',
});
export const MANAGE_HEADING_DESCRIPTOR = msg({
	message: 'Your subscription',
	comment: 'Heading of the subscription management panel on the Plutonium page.',
});
export const SHOWCASE_EXPRESSIONS_TITLE_DESCRIPTOR = msg({
	message: 'Your emojis, everywhere',
	comment: 'Plutonium page perk card title about using custom emojis and stickers in every community.',
});
export const SHOWCASE_EXPRESSIONS_BODY_DESCRIPTOR = msg({
	message: "Bring custom emojis and stickers from any of your communities into every chat and community you're in.",
	comment: 'Plutonium page perk card body about global emojis and stickers.',
});
export const SHOWCASE_PROFILE_TITLE_DESCRIPTOR = msg({
	message: 'A profile that stands out',
	comment: 'Plutonium page perk card title about profile customization.',
});
export const SHOWCASE_PROFILE_BODY_WITHOUT_TAG_DESCRIPTOR = msg({
	message: 'Get an animated avatar and banner, a subscriber badge and a separate profile for each community.',
	comment:
		'Plutonium page perk card body about profile perks, shown on instances where people sign in with a username and have no tag.',
});
export const SHOWCASE_PROFILE_BODY_DESCRIPTOR = msg({
	message:
		'Get an animated avatar and banner, a subscriber badge, the four-digit tag you want after your username{footnote}, and a separate profile for each community.',
	comment:
		'Plutonium page perk card body about profile perks. footnote is a clickable asterisk that points to a footnote about tags. Keep it directly after the word for username.',
});
export const SHOWCASE_STREAM_TITLE_DESCRIPTOR = msg({
	message: 'Stream in 4K at 60 FPS',
	comment: 'Plutonium page perk card title about stream quality.',
});
export const SHOWCASE_STREAM_BODY_DESCRIPTOR = msg({
	message: 'Share your screen or camera in up to 4K at 60 frames per second. Free accounts stream in up to 720p at 30.',
	comment: 'Plutonium page perk card body about stream quality.',
});
export const SHOWCASE_UPLOAD_TITLE_DESCRIPTOR = msg({
	message: 'Send files up to {size}',
	comment: 'Plutonium page perk card title about upload size. size is a formatted size such as 500 MB.',
});
export const SHOWCASE_UPLOAD_BODY_DESCRIPTOR = msg({
	message:
		'Share full-length videos and big files without shrinking them first. Free accounts can send up to {freeSize}.',
	comment: 'Plutonium page perk card body about upload size. freeSize is a formatted size such as 25 MB.',
});
export const COMPARE_TITLE_DESCRIPTOR = msg({
	message: 'Compare Free and {premiumProductName}',
	comment: 'Heading of the comparison table on the Plutonium page. premiumProductName is the paid tier name.',
});
export const FEATURE_COLUMN_DESCRIPTOR = msg({
	message: 'Feature',
	comment: 'Column header for perk names in the Plutonium page comparison table.',
});
export const FREE_COLUMN_DESCRIPTOR = msg({
	message: 'Free',
	comment: 'Column header for the free tier in the Plutonium page comparison table.',
});
export const AVAILABLE_DESCRIPTOR = msg({
	message: 'Available',
	comment: 'Screen reader text for a check mark in the Plutonium page comparison table.',
});
export const NOT_AVAILABLE_DESCRIPTOR = msg({
	message: 'Not available',
	comment: 'Screen reader text for a cross in the Plutonium page comparison table.',
});
export const TAG_FOOTNOTE_DESCRIPTOR = msg({
	message:
		"You can only pick a tag that nobody else with the same username already has. Usernames aren't case sensitive, so Mina#4821 and mina#4821 count as the same. The #0000 tag is reserved for {productName} Visionary members. {visionaryLink}.",
	comment:
		'Footnote on the Plutonium page about custom username tags. Keep Mina#4821, mina#4821 and #0000 unchanged. visionaryLink is a link whose text is "Learn more about Visionary".',
});
export const TAG_FOOTNOTE_LINK_DESCRIPTOR = msg({
	message: 'Learn more about Visionary',
	comment: 'Link text in the Plutonium page tag footnote. Opens the Visionary help article.',
});
export const TAG_FOOTNOTE_MARKER_DESCRIPTOR = msg({
	message: 'Footnote about custom tags',
	comment: 'Screen reader label for the asterisk that jumps to the custom tag footnote on the Plutonium page.',
});
export const CLOSING_TITLE_DESCRIPTOR = msg({
	message: 'Ready to upgrade?',
	comment: 'Heading of the closing call to action on the Plutonium page for people without the paid tier.',
});
export const CLOSING_BODY_DESCRIPTOR = msg({
	message: 'Pick a plan and your perks switch on the moment checkout finishes.',
	comment: 'Body of the closing call to action on the Plutonium page for people without the paid tier.',
});
export const CLOSING_GIFT_TITLE_DESCRIPTOR = msg({
	message: 'Keep your perks going',
	comment: 'Heading of the closing call to action on the Plutonium page for members on gifted time who can subscribe.',
});
export const CLOSING_GIFT_BODY_DESCRIPTOR = msg({
	message: 'Subscribe now. Your remaining gift time is added after your paid period.',
	comment: 'Body of the closing call to action on the Plutonium page for members on gifted time who can subscribe.',
});
export const CLOSING_MEMBER_TITLE_DESCRIPTOR = msg({
	message: 'Share the perks',
	comment: 'Heading of the closing call to action on the Plutonium page for members.',
});
export const CLOSING_MEMBER_BODY_DESCRIPTOR = msg({
	message: 'Gift {premiumProductName} to a friend for a month or a whole year. Gifts never renew.',
	comment:
		'Body of the closing call to action on the Plutonium page for members. premiumProductName is the paid tier name.',
});
export const PERK_CUSTOM_TAG_DESCRIPTOR = msg({
	message: 'Pick the 4-digit number after your username',
	comment: 'Plutonium page comparison row label for the custom username tag perk.',
});
export const PERK_PER_COMMUNITY_PROFILES_DESCRIPTOR = msg({
	message: 'A separate profile for each community',
	comment: 'Plutonium page comparison row label for per-community profiles.',
});
export const PERK_PROFILE_BADGE_DESCRIPTOR = msg({
	message: 'Subscriber badge on your profile',
	comment: 'Plutonium page comparison row label for the subscriber badge.',
});
export const PERK_VIDEO_BACKGROUNDS_DESCRIPTOR = msg({
	message: 'Video call backgrounds you can save',
	comment: 'Plutonium page comparison row label for saved video call backgrounds.',
});
export const PERK_COMMUNITIES_DESCRIPTOR = msg({
	message: 'Communities you can join',
	comment: 'Plutonium page comparison row label for the community limit.',
});
export const PERK_MESSAGE_CHARACTERS_DESCRIPTOR = msg({
	message: 'Characters in a single message',
	comment: 'Plutonium page comparison row label for the message length limit.',
});
export const PERK_BOOKMARKS_DESCRIPTOR = msg({
	message: 'Messages you can bookmark',
	comment: 'Plutonium page comparison row label for the bookmark limit.',
});
export const PERK_UPLOAD_SIZE_DESCRIPTOR = msg({
	message: 'Largest file you can upload',
	comment: 'Plutonium page comparison row label for the upload size limit.',
});
export const PERK_SAVED_MEDIA_DESCRIPTOR = msg({
	message: 'Media items you can save for later',
	comment: 'Plutonium page comparison row label for the saved media limit.',
});
export const PERK_ANIMATED_EMOJIS_DESCRIPTOR = msg({
	message: 'Use animated emojis in messages',
	comment: 'Plutonium page comparison row label for animated emoji use.',
});
export const PERK_GLOBAL_EXPRESSIONS_DESCRIPTOR = msg({
	message: 'Use custom emojis and stickers in any community',
	comment: 'Plutonium page comparison row label for global emojis and stickers.',
});
export const PERK_VIDEO_QUALITY_DESCRIPTOR = msg({
	message: 'Video call and screen share quality',
	comment: 'Plutonium page comparison row label for video quality.',
});
export const PERK_VIDEO_QUALITY_FREE_DESCRIPTOR = msg({
	message: 'Up to 720p at 30 FPS',
	comment: 'Plutonium page comparison value for free video quality. Keep 720p and FPS as written.',
});
export const PERK_VIDEO_QUALITY_PREMIUM_DESCRIPTOR = msg({
	message: 'Up to 4K at 60 FPS',
	comment: 'Plutonium page comparison value for paid video quality. Keep 4K and FPS as written.',
});
export const PERK_ANIMATED_PROFILE_DESCRIPTOR = msg({
	message: 'Animated avatar and profile banner',
	comment: 'Plutonium page comparison row label for animated avatars and banners.',
});
export const PERK_EARLY_ACCESS_DESCRIPTOR = msg({
	message: 'Early access to new features',
	comment: 'Plutonium page comparison row label for early access.',
});
export const PERK_CUSTOM_THEMES_DESCRIPTOR = msg({
	message: 'Custom themes for the app',
	comment: 'Plutonium page comparison row label for custom themes.',
});
export const PURCHASE_BLOCKED_CLAIM_DESCRIPTOR = msg({
	message: 'Claim your account to buy {premiumProductName}.',
	comment:
		'Plutonium page note under disabled buttons for unclaimed accounts. premiumProductName is the paid tier name.',
});
export const PURCHASE_BLOCKED_VERIFY_DESCRIPTOR = msg({
	message: 'Verify your email to buy {premiumProductName}.',
	comment:
		'Plutonium page note under disabled buttons for unverified accounts. premiumProductName is the paid tier name.',
});
export const PAGE_LABEL_DESCRIPTOR = msg({
	message: '{premiumProductName} page',
	comment: 'Accessible label of the Plutonium page region. premiumProductName is the paid tier name.',
});
export const BACK_DESCRIPTOR = msg({
	message: 'Back',
	comment: 'Accessible label of the back button on the Plutonium page in the mobile layout.',
});
