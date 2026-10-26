// SPDX-License-Identifier: AGPL-3.0-or-later

type OperatorPassTermsBlockKind = 'heading' | 'subheading' | 'paragraph' | 'bullet' | 'quote';

export interface OperatorPassTermsBlock {
	kind: OperatorPassTermsBlockKind;
	text: string;
}

export const OPERATOR_PASS_TERMS_TEXT = {
	'en-US': [
		{kind: 'heading', text: '1. What these terms are about'},
		{
			kind: 'paragraph',
			text: 'These terms apply when you buy or have the Operator Pass, and when someone passes one on to you. Checkout shows them before you pay, and we email them as a PDF with your purchase confirmation and, for a business purchase, with your invoice.',
		},
		{
			kind: 'paragraph',
			text: 'They add to our [terms of service](/terms) and replace part of it where they say so. The rest of the terms of service, the [community guidelines](/guidelines), and the [privacy policy](/privacy) still apply.',
		},
		{
			kind: 'paragraph',
			text: 'The Operator Pass is an optional one-time purchase that supports Fluxer and makes you an Operator. Fluxer Platform AB sells it on the web. One purchase can include up to 25 passes, and you can make as many purchases as you like. The price pays for your perks in section 2, and we deliver them to you as soon as you have paid. The bonus in section 3 comes with the pass but is not part of what you pay for.',
		},
		{kind: 'heading', text: '2. Your perks'},
		{kind: 'paragraph', text: 'In these terms, "your perks" means only these two things.'},
		{kind: 'bullet', text: "Your profile's Operator badge, showing you have the pass and since when."},
		{
			kind: 'bullet',
			text: "Your listing on the [Operators' wall](/#operators) on the Fluxer homepage. The listing shows the name and avatar you chose once we have approved them. A business can instead show its company name and logo, as section 2.2 describes.",
		},
		{
			kind: 'paragraph',
			text: 'Your badge and wall count includes valid passes you have plus valid passes you bought and passed on. Each pass counts once per account, for both buyer and recipient.',
		},
		{
			kind: 'paragraph',
			text: 'Paused passes do not count. Ended passes stop counting for both accounts. Keep at least one active pass for your badge, wall listing and other benefits.',
		},
		{
			kind: 'paragraph',
			text: 'You can hide the count. Settings show it separately from passes you have.',
		},
		{
			kind: 'paragraph',
			text: 'A larger count changes only your badge number and your wall size and position. It gives no extra access, support or priority, and no more of the bonus in section 3.',
		},
		{
			kind: 'paragraph',
			text: 'The Operator Pass is digital content. We deliver all passes you bought in full once your payment goes through and we have emailed the confirmation, in moments.',
		},
		{
			kind: 'paragraph',
			text: 'What we deliver is the badge and your right to a listing on the wall. At that point the badge turns on, your listing goes up on the wall at its size and place unless you chose not to be listed, and the count includes every pass you bought. Nothing more is left for us to deliver. Showing the name and avatar you chose depends only on our check in section 2.1, and section 5 adds our promise not to take your perks down.',
		},
		{kind: 'subheading', text: '2.1 Your name and avatar'},
		{
			kind: 'paragraph',
			text: 'At checkout you choose the name and avatar for your listing, or choose not to be listed, and the badge turns on either way. We suggest your Fluxer display name and avatar, but any display name or nickname works, and you never need your legal name.',
		},
		{
			kind: 'paragraph',
			text: 'We check every name and avatar before it shows on the wall, and we aim to check what you chose within 72 hours. Your listing goes up the moment you pay and shows your username and the default avatar until we approve the name and avatar you chose, or the word "Operator" if your username breaks the rules below.',
		},
		{
			kind: 'paragraph',
			text: 'Unless you turn it off in your settings, your listing follows changes to your Fluxer display name and avatar, and we check each change the same way.',
		},
		{
			kind: 'paragraph',
			text: 'Until we approve a change, the wall keeps showing the last version we approved, not your live profile. We keep copies of the names and avatars you send us for the wall and their history, as the [Operator Pass privacy notice](/operator-pass-privacy) describes.',
		},
		{
			kind: 'paragraph',
			text: "We can decline to show a name or avatar on the Operators' wall, or any change to one, if it breaks the [community guidelines](/guidelines), pretends to be someone else, uses a name or an image that you have no right to use, or is not suitable for the youngest people who are allowed to use Fluxer under section 1 of the [terms of service](/terms#1-agreement-and-eligibility).",
		},
		{
			kind: 'paragraph',
			text: 'When we decline one, the wall keeps showing the last version that we approved, or the fallback described above if there is no such version. You stay on the wall with the same size and place as before, your badge stays, and nothing is withdrawn or refunded. We tell you what we did, why, and how to appeal, as section 6.2 of the [terms of service](/terms#6-2-enforcement-by-fluxer) describes, and you can send us a new name or avatar at any time.',
		},
		{
			kind: 'paragraph',
			text: 'We can also shorten a long name or adjust how a name or avatar is shown so that it fits the wall, for example by cutting it at a set length, removing characters that do not display, or cropping the avatar. We keep it recognizable as yours.',
		},
		{
			kind: 'paragraph',
			text: 'When we decline or replace a name or avatar, that is not a removal of pass benefits, and we take you off the wall only under section 8 of these terms.',
		},
		{
			kind: 'paragraph',
			text: 'At any time, in your settings or through the support route on our [company information page](/company-information), you can change your name or avatar on the wall, leave the wall, come back to it, or hide the badge. Choosing not to be listed, leaving the wall, or hiding the badge does not undo delivery, so it does not change the price and gives no refund.',
		},
		{
			kind: 'paragraph',
			text: 'Today Operators with a higher count appear larger and first. The four sizes are for counts of 1, 2 to 4, 5 to 9 and 10 or more. Within each size we order Operators by the earliest delivery date of a pass that still counts for them, earliest first.',
		},
		{
			kind: 'paragraph',
			text: 'For purchases, we use the original delivery to you, even if you later passed the pass on. For passes from someone else, we use the delivery to your account. The badge date follows the same rule. Every wall size shows a name and avatar, and business listings show a company name and logo, as section 2.2 describes.',
		},
		{
			kind: 'paragraph',
			text: 'Your avatar size and position are not guaranteed. They change as the counts and dates of Operators change. Passing on a pass does not itself reduce your count or change its date, provided you keep at least one active pass. Keeping you on the wall for life means keeping your listing there, not its size or place in the order.',
		},
		{kind: 'subheading', text: '2.2 Listing your business'},
		{
			kind: 'paragraph',
			text: 'If you bought passes for your business under section 4, you can ask us to list your business with its company name and logo instead of a name and avatar. If your VAT ID is from an EU country other than Sweden, VIES must have confirmed it.',
		},
		{
			kind: 'paragraph',
			text: 'Email us through the support route on our [company information page](/company-information) from the email address of the Fluxer account that has the passes, and send the company name and a square logo. We can ask you to show that you can act for the business.',
		},
		{
			kind: 'paragraph',
			text: 'Business listings appear together in their own part of the wall, after the listings of people. Every business listing has the same size, and we order them by first pass date, earliest first. Business listings do not appear on our self-hosting page.',
		},
		{
			kind: 'paragraph',
			text: 'A business listing uses the same count as a personal listing. Passes bought by its account and passed on to staff count for both the business account and the recipients, as long as each account still holds at least one active pass.',
		},
		{
			kind: 'paragraph',
			text: "All business listings stay the same size. Their order follows the delivery-date rule in section 2.1. A business listing replaces the personal listing of the account that bought the passes, does not follow changes to that account's profile, and has no link.",
		},
		{
			kind: 'paragraph',
			text: 'A business listing shows only that the business bought passes. It is not an endorsement or a partnership either way, and it gives the business no say in what we build.',
		},
		{
			kind: 'paragraph',
			text: 'Section 2.1 applies to the company name and logo as it does to a name and avatar, including our check before anything shows, the reasons we can decline it, and how we fit it to the wall. We can also decline a business listing, or end one later, if showing it could make people think we recommend the business or work with it.',
		},
		{
			kind: 'paragraph',
			text: 'Examples are a business in gambling, adult content, weapons, drugs or political campaigning, or one that sells followers, accounts, reviews, links, academic work or spam. Until we approve a business listing, and whenever we decline or end one, the account keeps its listing with a name and avatar, and we tell you why.',
		},
		{
			kind: 'paragraph',
			text: 'By sending us a logo, you confirm that you can act for the business and that it has the rights to let us show the name and logo. You let us show them free of charge and worldwide on the wall, and in pictures and recordings of our website that show the wall, while the business listing is up. This gives us no other right to use them.',
		},
		{
			kind: 'paragraph',
			text: 'If someone tells us that a name or logo on the wall infringes their rights, we can hide it while we look into it. The listing then shows the last name and avatar we approved, or the username and the default avatar. We tell you what we did and why, and you can reply or appeal as section 2.1 describes. This is not a removal of pass benefits.',
		},
		{
			kind: 'paragraph',
			text: 'To change the logo or go back to a name and avatar, ask us the same way. You can also leave the wall at any time, or come back to it, as section 2.1 describes.',
		},
		{
			kind: 'paragraph',
			text: "If you change your wall name or avatar in your settings, the listing becomes a personal listing again once we approve the change. If we ever stop showing business listings, each one goes back to the account's listing with a name and avatar.",
		},
		{kind: 'heading', text: '3. The bonus, and what you do not get'},
		{kind: 'paragraph', text: 'While we choose to run it, Operators also get a bonus with these four parts.'},
		{
			kind: 'bullet',
			text: 'The Operator role in the Fluxer Labs community. It shows Operators as their own group in the member list and gives access to the private #operators channel, where Hampus Kraft, creator of Fluxer, takes part when he can. Operators can talk there about Fluxer, self-hosting, the API and bots, and any instance they run.',
		},
		{
			kind: 'bullet',
			text: "Invitations to test new features before they ship. Pre-release features can have bugs. Try them where a bug can't hurt you or your users.",
		},
		{
			kind: 'bullet',
			text: 'Your name in "The Operators" list for the year, such as "The Operators of 2026", if you choose it in settings. We publish it for free in a tagged Fluxer release.',
		},
		{
			kind: 'bullet',
			text: 'Once we have a list of Fluxer instances on our website, an Operator mark on the listing of an instance you run. Anyone who follows the listing rules can add their instance to the list for free, and you never need the pass to be on it. The list is not out yet, and we may change the mark or decide not to offer it.',
		},
		{
			kind: 'paragraph',
			text: 'Every Operator gets the same bonus, however many passes they have. The bonus is not part of what you pay for, and these terms give you no right to it.',
		},
		{
			kind: 'paragraph',
			text: 'We can change, pause, or end any part of the bonus for everyone at any time, and you get no refund or other compensation when we do. When we end a part of the bonus for everyone, we tell all Operators about it in Fluxer Labs or by email. We take a part of the bonus away from a single person only as section 8 allows.',
		},
		{
			kind: 'paragraph',
			text: 'To use the #operators channel and the Operator role, you need to be a member of the Fluxer Labs community with the account that has the Operator Pass.',
		},
		{
			kind: 'paragraph',
			text: 'The channel is a place to talk, and the pass is not a support contract. There is no guaranteed response time or service level, and we do not promise that any question gets an answer, that Hampus takes part in any given conversation, that every problem can be fixed, or that a feature you ask for gets built. We do not log in to, run, or look after any instance you run, and you stay responsible for it and its data.',
		},
		{kind: 'subheading', text: '3.1 The Operators release list'},
		{
			kind: 'paragraph',
			text: 'Your name goes on every yearly list we publish while you have the pass, are listed on the wall and have "Add my name to the yearly Operators list" on in your settings. The setting is off until you turn it on. The list uses the name on your wall listing at the time, and it goes into the release notes and into a file in the Fluxer source code repository.',
		},
		{
			kind: 'paragraph',
			text: 'If you ask us to erase your name after a list has been published, we leave it out of all future releases, edit the published release notes to remove it, and remove it from the list file in the next change to the source code. Earlier versions in the history of the source code, and copies that others have made, cannot be recalled.',
		},
		{kind: 'subheading', text: '3.2 What the pass does not include'},
		{
			kind: 'paragraph',
			text: 'The pass unlocks nothing in the Fluxer software. Self-hosted Fluxer works the same with or without the pass and it stays free to run, and losing the pass never affects an instance that you run. The pass includes no Plutonium benefits, no hosting, and no license or other rights in the software. The pass has no cash value.',
		},
		{kind: 'heading', text: '4. Price and payment'},
		{
			kind: 'paragraph',
			text: 'The price of a pass is shown on the [Operator Pass page](/pass) and at checkout before you pay, including any tax that applies. You pay once, and nothing renews, and section 4.1 of the [terms of service](/terms#4-1-payments-and-renewal) says how payment works. If the price changes after you buy, we do not charge you more, and we do not refund you the difference either.',
		},
		{
			kind: 'paragraph',
			text: 'At checkout you can also add a monthly donation. It is separate from the pass, these terms do not apply to it, and you can cancel it any time from the [donation page](/donate/manage).',
		},
		{
			kind: 'paragraph',
			text: 'If you buy for your business, check "I\'m buying for a business" at checkout to confirm that you are buying the passes for your trade, business, or profession. Checkout then asks for your company name, address, and VAT ID, and you get an invoice with them on it. The total price you pay is the same as for any other buyer.',
		},
		{
			kind: 'paragraph',
			text: 'If your VAT ID is from an EU country other than Sweden, reverse charge applies to your purchase, and you account for the VAT yourself, as the invoice says. A business in Sweden pays Swedish VAT, which is already included in the price. Outside the EU, any tax that applies is also included in the price, and your invoice shows it.',
		},
		{
			kind: 'paragraph',
			text: "We check EU VAT IDs with the European Commission's VIES service after you have paid. If VIES does not confirm your VAT ID, the total price you paid stays the same, you still bought for your business, and only our VAT accounting changes.",
		},
		{
			kind: 'paragraph',
			text: 'Your passes go to the Fluxer account you buy with. Your VAT ID appears only on your invoice. Your company name shows on the wall only if you choose it as your wall name or ask for a business listing under [section 2.2](/operator-pass-terms#2-2-listing-your-business), and never on your badge.',
		},
		{
			kind: 'paragraph',
			text: "You can pass each pass on to a staff member's account under section 11 once you are friends on Fluxer, and doing so does not change your invoice.",
		},
		{kind: 'heading', text: '5. How long your perks last'},
		{
			kind: 'paragraph',
			text: 'You keep your perks "for life". In these terms, for life means for as long as we run the official Fluxer service and website, never the lifetime of a person.',
		},
		{
			kind: 'paragraph',
			text: 'For that time we promise not to take down the badge or your listing, unless the pass ends under section 8, 9, or 10, you pass on your only pass under section 11, or you choose to hide them. Your perks do not depend on how often you use Fluxer, and we never delete an account that has the pass because of inactivity.',
		},
		{
			kind: 'paragraph',
			text: 'If Fluxer Platform AB or the official Fluxer service is sold or transferred to someone else, the new owner must take over every existing pass on these same terms.',
		},
		{kind: 'heading', text: '6. Changes to your perks'},
		{
			kind: 'paragraph',
			text: 'We can change how the badge and wall look, where the wall appears on the homepage, and the order and layout of the list, as long as your listing still shows your name and avatar, or the company name and logo of a business listing as section 2.2 describes, and the badge stays on your profile. Changes cost you nothing.',
		},
		{
			kind: 'paragraph',
			text: 'If we decide to close the official Fluxer service and website, we tell Operators by email at least 30 days ahead. If the law or the security of Fluxer makes us take the badge or the wall down while we still run the official service, we tell you as early as we can. Nothing in this section limits the rights the law gives you, which section 7.3 describes.',
		},
		{kind: 'heading', text: '7. Refunds and the right of withdrawal'},
		{kind: 'subheading', text: '7.1 Withdrawal'},
		{
			kind: 'paragraph',
			text: 'In the EU, the EEA, the UK, and some other places, consumers can withdraw from an online purchase within a set time period without giving any reason. For digital content, that right ends once delivery starts, but only if you asked for delivery during the withdrawal period and accepted that you would lose the right.',
		},
		{
			kind: 'paragraph',
			text: 'Before you pay, every buyer, whether buying as a consumer or for a business, has to check a box that is never checked for you, and you cannot buy a pass without checking that box. By checking it, you ask us to deliver every pass in your purchase as soon as you pay, and you confirm that you understand that you lose your right to withdraw once they are delivered and that the bonus is not part of the price.',
		},
		{
			kind: 'paragraph',
			text: 'Before we deliver, we email you a confirmation that repeats what you agreed to, with the time you agreed, the price, and these terms. We record your consent, the time, the version of these terms, your IP address, your browser details, and your account.',
		},
		{
			kind: 'paragraph',
			text: 'Once a pass is delivered, you cannot withdraw from it. If we have not delivered it yet, for example because of a technical problem, you can still withdraw with "Withdraw from contract here" next to the purchase in your settings or by telling us in any other clear way through the support route on our [company information page](/company-information).',
		},
		{
			kind: 'paragraph',
			text: 'We then refund you in full within 14 days, to the same payment method, and if the purchase came with an invoice, we also send a credit note that refers to it.',
		},
		{
			kind: 'paragraph',
			text: 'If a court or consumer authority where you live decides that you still have a right of withdrawal after delivery, we follow that decision and refund you in full.',
		},
		{
			kind: 'paragraph',
			text: 'Someone who receives a pass under section 11 does not pay for it and has no right of withdrawal. If you buy a pass for your business, you have no right of withdrawal under this section, even if VIES does not confirm your VAT ID. Section 12 adds what applies in some countries, and where it says so, it applies to business purchases too.',
		},
		{kind: 'subheading', text: '7.2 Withdrawal form'},
		{
			kind: 'paragraph',
			text: 'While you still have a right of withdrawal, you can copy this form, fill it in, and send it to <support@fluxer.com>. You do not have to use the form to withdraw.',
		},
		{
			kind: 'quote',
			text: 'To Fluxer Platform AB, c/o Embassy House, Östgötagatan 12, 116 25 Stockholm, Sweden, support@fluxer.com. I withdraw from my contract for the Operator Pass. Bought on (date). Name. Email address of the Fluxer account. Date.',
		},
		{kind: 'subheading', text: '7.3 After delivery'},
		{
			kind: 'paragraph',
			text: 'Except where the law where you live gives you a right that cannot be waived, purchases are final and we do not give refunds, though we may choose to. For the pass, this section replaces sections 4.2 and 4.3 of the [terms of service](/terms#4-2-refunds).',
		},
		{
			kind: 'paragraph',
			text: 'Your statutory rights are not affected. You still get a refund or another remedy when the law gives you one, for example when we charge you the wrong amount or more than once, when someone used your payment method without your permission, or when your perks are not delivered or are not as described here or on the pass page.',
		},
		{kind: 'heading', text: '8. When we remove pass benefits'},
		{
			kind: 'paragraph',
			text: 'In this section and section 9, "pass benefits" means your perks and your access to the bonus. We can remove some or all of your pass benefits, for a limited time or permanently, if you break these terms, the [terms of service](/terms), or the [community guidelines](/guidelines) anywhere on the official Fluxer service, including Fluxer Labs.',
		},
		{
			kind: 'paragraph',
			text: 'That includes misusing #operators or the Operator role, such as by harassing the team or other operators, sharing what others post there without their permission, advertising, or letting someone else into the channel. It also includes using your role or badge to talk down to others or to act as if you have authority over them.',
		},
		{
			kind: 'paragraph',
			text: 'We judge a breach by the rules in force at the time.',
		},
		{
			kind: 'paragraph',
			text: 'For a minor breach, we start with a warning or a removal for a limited time, as section 9 of the [community guidelines](/guidelines#9-enforcement-and-appeals) describes. We remove the pass permanently only for a serious breach, such as threats, harassment, illegal content, or fraud, or for a breach after we have already removed your pass benefits for a limited time.',
		},
		{
			kind: 'paragraph',
			text: 'Before we remove the pass permanently, we tell you why and give you 7 days to reply, unless the breach is serious enough that we must act at once.',
		},
		{
			kind: 'paragraph',
			text: "If we terminate your Fluxer account because you broke our terms or the community guidelines, the pass ends with it. We take you off the Operators' wall only for a serious breach or for repeated breaches, and never just because we declined a name or avatar under [section 2.1](/operator-pass-terms#2-1-your-name-and-avatar). We remove pass benefits only for the reasons in these terms.",
		},
		{
			kind: 'paragraph',
			text: 'When we remove pass benefits, you get the statement of reasons that section 6.2 of the [terms of service](/terms#6-2-enforcement-by-fluxer) describes, including how long the removal will last, and you can appeal under that section, including through the out-of-court route that our [DSA dispute help](/help/dsa-dispute-resolution) describes. Pass benefits that we remove are not refunded.',
		},
		{
			kind: 'paragraph',
			text: 'If we remove the pass permanently, you cannot buy a pass or receive one again. Removing pass benefits does not suspend your Fluxer account by itself, and any measure on your account follows section 6.2 of the terms of service.',
		},
		{
			kind: 'paragraph',
			text: 'If the appeal shows that we got it wrong, your pass benefits come back with the original date on your badge. They also come back if a measure that we took on your Fluxer account together with the removal is later reversed on appeal.',
		},
		{
			kind: 'paragraph',
			text: 'As a consumer, you can also take a dispute about a removal to the Swedish National Board for Consumer Disputes, as our [company information page](/company-information#consumer-disputes) explains.',
		},
		{kind: 'heading', text: '9. Payment disputes and chargebacks'},
		{
			kind: 'paragraph',
			text: 'You can always dispute a charge with your bank or card company, and nothing in these terms stops you. If something is wrong with a pass payment, the fastest fix is to tell us through the support route on our [company information page](/company-information).',
		},
		{
			kind: 'paragraph',
			text: 'While a dispute about any payment you made to us is open, you cannot buy a pass. While a dispute about a pass payment is open, you also cannot buy anything else on Fluxer, and we pause the pass. You can buy again as soon as the dispute closes, and we may ask you for the information that we need to respond to it.',
		},
		{
			kind: 'paragraph',
			text: 'When we believe a dispute is wrong, we contest it and show your bank the record of your consent in section 7.1, the business details you gave at checkout if you bought for your business, the confirmation email, and when we delivered the pass.',
		},
		{
			kind: 'paragraph',
			text: 'If the dispute closes without the money being returned to you, including when you withdraw it, we restore the pass and nothing happens to your account.',
		},
		{
			kind: 'paragraph',
			text: 'If the money goes back to you, every pass paid for by that payment ends and stops counting. This also applies to passes you passed on, as section 11 describes.',
		},
		{
			kind: 'paragraph',
			text: 'Other payments are not affected. When an account’s last pass ends, we take its badge and listing down. If a pass was passed on, the buyer loses the credit for it too.',
		},
		{
			kind: 'paragraph',
			text: 'A person on our team checks every dispute about a pass payment while it is open. A dispute is unjustified only when you or someone you allowed made the payment, we charged the right amount, we delivered the pass as these terms and the pass page describe, we had not refunded the payment, and your right of withdrawal under [section 7.1](/operator-pass-terms#7-1-withdrawal) or section 12 had ended when you disputed the payment.',
		},
		{
			kind: 'paragraph',
			text: 'A dispute is always justified, and never leads to any action against your Fluxer account beyond the pause described above, in each of these cases.',
		},
		{kind: 'bullet', text: 'Someone used your payment method without your permission.'},
		{kind: 'bullet', text: 'We charged you the wrong amount or more than once.'},
		{kind: 'bullet', text: 'We had already refunded the payment.'},
		{kind: 'bullet', text: 'We did not deliver the pass, or it was not as described.'},
		{kind: 'bullet', text: 'You disputed while you still had a right of withdrawal.'},
		{kind: 'bullet', text: 'You withdrew under section 7.1 or section 12 and we did not refund you.'},
		{
			kind: 'bullet',
			text: 'You dispute a removal under [section 8](/operator-pass-terms#8-when-we-remove-perks) that you believe was wrong.',
		},
		{
			kind: 'bullet',
			text: 'The ground that your bank or card company gives for the dispute is that the payment was not made by you or by someone that you allowed, that we charged you the wrong amount or more than once, that we did not process a refund that we owed you, or that the pass was not delivered as described.',
		},
		{
			kind: 'paragraph',
			text: 'If we find a dispute unjustified, we tell you by email and in a message in Fluxer, with our reasons and a date a few days before we have to answer your bank. If the dispute is not withdrawn before that date and it ends with the money returned to you, we permanently delete the Fluxer account that made the payment, and it cannot be restored. Neither signing in nor a request to support stops the deletion.',
		},
		{
			kind: 'paragraph',
			text: 'From the time we close the account for this, you may not create or use other Fluxer accounts, and we block its email address from signing up again.',
		},
		{
			kind: 'paragraph',
			text: 'We delete the account 60 days after the dispute ends, and until then you can still ask for a copy of your data, as section 10 of the [privacy policy](/privacy#10-your-rights) describes.',
		},
		{
			kind: 'paragraph',
			text: 'You can appeal under section 6.2 of the [terms of service](/terms#6-2-enforcement-by-fluxer), and if the appeal shows the dispute was justified, we cancel the deletion and lift the block.',
		},
		{
			kind: 'paragraph',
			text: 'If our payment provider warns us a pass payment was likely fraudulent, we refund it, report it to the provider, and block the payment details used, and the pass that payment paid for ends. The warning alone never leads to action against your account.',
		},
		{kind: 'heading', text: '10. Deleting your account'},
		{
			kind: 'paragraph',
			text: 'A pass belongs to the account that has it. If you delete your account, every pass on it ends when the deletion completes after the grace period in section 6.1 of the [terms of service](/terms#6-1-disabling-or-deleting-your-account), any offer you made under section 11 is withdrawn, and we take your listing down. Passes are not refunded when you delete your account. To keep a pass you have not passed on yet, pass it on under section 11 before the deletion completes.',
		},
		{kind: 'heading', text: '11. Pass it on'},
		{
			kind: 'paragraph',
			text: 'You can offer a pass that you bought to another Fluxer account. That account must be your friend on Fluxer when you offer the pass and when they accept it. Each pass can be passed on only once, and a pass that you received from someone else cannot be passed on again. A pass that came with Visionary cannot be passed on at all, and a pass cannot go to an account that has already lost the pass under section 8.',
		},
		{
			kind: 'paragraph',
			text: 'Until the other person accepts, the pass stays yours and you can take the offer back. They accept these terms and choose a wall name or choose not to be listed. We then deliver the pass to their account, subject to the waiting period below.',
		},
		{
			kind: 'paragraph',
			text: 'Their badge turns on and their listing goes up, or their count goes up if they already have a pass. From then on, these terms apply to them for that pass.',
		},
		{
			kind: 'paragraph',
			text: 'After delivery, the pass still counts for you as its buyer if you keep at least one active pass. It also counts for the recipient, from the date we deliver it to them. This does not give you any ownership of, or control over, the pass you passed on.',
		},
		{
			kind: 'paragraph',
			text: 'A pass moves only once 72 hours have passed since the payment for it, and only if that payment has no fraud warning, open dispute, or refund. Section 12 sets a longer wait in some countries. Until then, the other person sees when the pass will arrive.',
		},
		{
			kind: 'paragraph',
			text: 'If you pass on your only remaining pass, your Operator badge and your listing on the wall go away, and we ask you to confirm before that happens. Credit for passes you bought does not keep these benefits active on its own.',
		},
		{
			kind: 'paragraph',
			text: 'Once you have an active pass again, valid purchases passed on count too.',
		},
		{
			kind: 'paragraph',
			text: 'Passing a pass on is not a sale. You get no refund or payment for it, and the other person pays nothing. The purchase stays yours. Any refund the law or these terms give for it goes to you, to the payment method you used. If the payment for a pass is refunded or reversed, that pass ends for whoever has it, as section 9 describes. Sections 2, 5, 6, 7.3, 8, and 10 apply to whoever has a passed-on pass as if they had bought it.',
		},
		{
			kind: 'paragraph',
			text: 'That person has no right of withdrawal of their own, as section 7.1 says, and can decline the offer instead. Once a pass has moved to another account, it belongs to that account. A business that passes a pass on to a staff member cannot take it back, and the pass stays with that person if they leave the business.',
		},
		{kind: 'heading', text: '12. Rules for some countries'},
		{
			kind: 'paragraph',
			text: 'Some countries give consumers a right to withdraw from an online purchase that cannot be waived, even after delivery. Where this section lists such a country, the right that it describes applies in place of the waiver in section 7.1.',
		},
		{
			kind: 'paragraph',
			text: '**Brazil.** If your billing address or your card is from Brazil, or if you pay in Brazilian reais, you can withdraw from your purchase within 7 days of the payment, whether you buy as a consumer or for your business, and we still deliver your passes to you right away. To withdraw from it, use "Cancel" next to the pass in your settings, or tell us through the support route that is listed on our [company information page](/company-information).',
		},
		{
			kind: 'paragraph',
			text: 'We confirm your withdrawal at once, refund the full price of that pass right away, to the same payment method or by Pix refund, and the pass ends. If the purchase came with an invoice, we also send you a credit note that refers to it. A pass bought this way can be passed on under section 11 only once those 7 days have passed.',
		},
		{
			kind: 'paragraph',
			text: '**Australia.** If you are a consumer in Australia, section 7.3 means we give no refunds if you change your mind. Our perks come with guarantees that cannot be excluded under the Australian Consumer Law, and you are entitled to a refund for a major failure.',
		},
		{kind: 'heading', text: '13. Visionary'},
		{
			kind: 'paragraph',
			text: "Visionary includes an Operator Pass, and we'll email every Visionary about turning on its perks. Until you do, these terms don't apply to you and Visionary stays as it is.",
		},
		{kind: 'heading', text: '14. Changes and contact'},
		{
			kind: 'paragraph',
			text: 'We change these terms the same way we change the [terms of service](/terms#9-1-changes-to-these-terms), as its section 9.1 describes, and record every change in our [changelog](/changelog). A change that leaves you worse off applies to a pass you already have only if the law requires it or you agree.',
		},
		{
			kind: 'paragraph',
			text: 'Where a translation of these terms differs from the English, the version more favorable to you applies. Every contact route is on our [company information page](/company-information).',
		},
	],
	'sv-SE': [
		{kind: 'heading', text: '1. Vad de här villkoren gäller'},
		{
			kind: 'paragraph',
			text: 'De här villkoren gäller när du köper eller har Operator Pass, och när någon lämnar vidare ett till dig. Kassan visar dem innan du betalar, och vi mejlar dem som PDF tillsammans med bekräftelsen på köpet och, vid ett köp för företag, med din faktura.',
		},
		{
			kind: 'paragraph',
			text: 'De kompletterar våra [användarvillkor](/terms), och där det står så ersätter de en del av användarvillkoren. Allt annat i användarvillkoren, [communityriktlinjerna](/guidelines) och [integritetspolicyn](/privacy) gäller fortfarande.',
		},
		{
			kind: 'paragraph',
			text: 'Operator Pass är ett frivilligt engångsköp som stöttar Fluxer och gör dig till Operator. Fluxer Platform AB säljer det på webben. Ett köp kan innehålla upp till 25 pass, och du kan göra så många köp du vill. Priset betalar för dina förmåner i avsnitt 2, och vi levererar dem till dig så fort du har betalat. Bonusen i avsnitt 3 följer med passet men ingår inte i det du betalar för.',
		},
		{kind: 'heading', text: '2. Dina förmåner'},
		{kind: 'paragraph', text: 'I de här villkoren betyder "dina förmåner" bara de här två sakerna.'},
		{kind: 'bullet', text: 'Operator-märket på din profil, som visar att du har passet och sedan när.'},
		{
			kind: 'bullet',
			text: 'Din plats på [Operators-väggen](/#operators) på Fluxers startsida. Platsen visar det namn och den avatar som du har valt när vi har godkänt dem. Ett företag kan i stället visa sitt namn och sin logotyp, som avsnitt 2.2 beskriver.',
		},
		{
			kind: 'paragraph',
			text: 'Antalet på ditt märke och din plats på väggen omfattar giltiga pass som du har kvar plus giltiga pass som du har köpt och lämnat vidare. Varje pass räknas bara en gång för dig. Ett pass som du lämnar vidare räknas också för mottagaren.',
		},
		{
			kind: 'paragraph',
			text: 'Pausade pass räknas inte förrän de återupptas, och pass som upphör räknas inte längre för något av kontona. Du måste fortfarande ha kvar minst ett aktivt pass för att behålla ditt märke, din plats på väggen och övriga förmåner.',
		},
		{
			kind: 'paragraph',
			text: 'Du kan dölja det offentliga antalet. Dina inställningar visar det offentliga antalet separat från hur många pass du har kvar.',
		},
		{
			kind: 'paragraph',
			text: 'Ett högre antal ändrar bara siffran på märket och storleken och placeringen på din plats på väggen. Det ger dig ingen extra tillgång, support eller förtur och inte mer av bonusen i avsnitt 3.',
		},
		{
			kind: 'paragraph',
			text: 'Operator Pass är digitalt innehåll. Vi levererar alla pass du har köpt i sin helhet när din betalning har gått igenom och vi har mejlat bekräftelsen, inom några ögonblick.',
		},
		{
			kind: 'paragraph',
			text: 'Det vi levererar är märket och din rätt till en plats på väggen. Då slås märket på, din plats kommer upp på väggen med sin storlek och placering om du inte har valt att inte stå med, och antalet räknar med varje pass du köpte. Därefter finns inget kvar för oss att leverera. Att det namn och den avatar som du har valt visas beror bara på vår granskning i avsnitt 2.1, och avsnitt 5 lägger till vårt löfte att inte ta bort dina förmåner.',
		},
		{kind: 'subheading', text: '2.1 Ditt namn och din avatar'},
		{
			kind: 'paragraph',
			text: 'I kassan väljer du namn och avatar för din plats på väggen, eller väljer att inte stå med, och märket slås på i båda fallen. Vi föreslår ditt visningsnamn och din avatar på Fluxer, men vilket visningsnamn eller smeknamn som helst fungerar, och du behöver aldrig använda ditt juridiska namn.',
		},
		{
			kind: 'paragraph',
			text: 'Vi granskar varje namn och avatar innan de visas på väggen, och vi strävar efter att granska det du har valt inom 72 timmar. Din plats kommer upp på väggen så fort du har betalat och visar ditt användarnamn och standardavataren tills vi har godkänt det namn och den avatar som du har valt, eller ordet "Operator" om ditt användarnamn bryter mot reglerna nedan.',
		},
		{
			kind: 'paragraph',
			text: 'Om du inte stänger av det i dina inställningar följer din plats på väggen ändringar av ditt visningsnamn och din avatar på Fluxer, och vi granskar varje ändring på samma sätt.',
		},
		{
			kind: 'paragraph',
			text: 'Tills vi har godkänt en ändring visar väggen den senaste version som vi har godkänt. Väggen visar vår godkända kopia, inte din aktuella profil. Vi sparar kopior av de namn och avatarer du skickar oss för väggen och deras historik, som [integritetsmeddelandet för Operator Pass](/operator-pass-privacy) beskriver.',
		},
		{
			kind: 'paragraph',
			text: 'Vi kan avstå från att visa ett namn eller en avatar på Operators-väggen, eller någon ändring av dem, om det bryter mot [communityriktlinjerna](/guidelines), utger sig för att vara någon annan, använder ett namn eller en bild som du inte har rätt att använda, eller inte passar de yngsta som får använda Fluxer enligt avsnitt 1 i [användarvillkoren](/terms#1-agreement-and-eligibility).',
		},
		{
			kind: 'paragraph',
			text: 'När vi avstår från att visa något fortsätter väggen att visa den senaste version som vi har godkänt, eller reservvisningen ovan om det inte finns någon sådan version. Du står kvar på väggen med samma storlek och placering, ditt märke finns kvar, och inget dras in eller betalas tillbaka. Vi berättar vad vi har gjort, varför och hur du överklagar, som avsnitt 6.2 i [användarvillkoren](/terms#6-2-enforcement-by-fluxer) beskriver, och du kan när som helst skicka ett nytt namn eller en ny avatar.',
		},
		{
			kind: 'paragraph',
			text: 'Vi kan också korta ett långt namn eller justera hur ett namn eller en avatar visas så att det passar väggen, till exempel genom att klippa av det vid en viss längd, ta bort tecken som inte går att visa eller beskära avataren. Vi ser till att det fortfarande går att känna igen som ditt.',
		},
		{
			kind: 'paragraph',
			text: 'När vi avstår från att visa eller byter ut ett namn eller en avatar är det ingen borttagning av passets förmåner, och vi tar bara bort dig från väggen enligt avsnitt 8 i de här villkoren.',
		},
		{
			kind: 'paragraph',
			text: 'När som helst, i dina inställningar eller via supportvägen på vår [sida med företagsinformation](/company-information), kan du ändra ditt namn eller din avatar på väggen, lämna väggen, komma tillbaka till den eller dölja märket. Att du väljer att inte stå med, lämnar väggen eller döljer märket gör inte leveransen ogjord, så det ändrar inte priset och ger ingen återbetalning.',
		},
		{
			kind: 'paragraph',
			text: 'I dag visas Operators med ett högre antal större och först. De fyra storlekarna gäller antal på 1, 2 till 4, 5 till 9 och 10 eller fler. Inom varje storlek ordnar vi Operators efter det tidigaste leveransdatumet för ett pass som fortfarande räknas för dem, tidigast först. För ett pass som du har köpt är det när vi först levererade det till dig, även om du senare lämnade det vidare. För ett pass som du har fått av någon annan är det när vi levererade det till ditt konto. Datumet på ditt märke följer samma regel. Platser av alla storlekar visar sitt namn och sin avatar, och företagsplatser visar ett företagsnamn och en logotyp, som avsnitt 2.2 beskriver.',
		},
		{
			kind: 'paragraph',
			text: 'Storleken på din avatar och din placering är inte garanterade. De ändras när antalen och datumen för Operators ändras. Att lämna vidare ett pass minskar inte i sig ditt antal eller ändrar dess datum, förutsatt att du har kvar minst ett aktivt pass. Att du står kvar på väggen för livet betyder att du står kvar på den, inte att du behåller samma storlek eller placering i ordningen.',
		},
		{kind: 'subheading', text: '2.2 Visa ditt företag'},
		{
			kind: 'paragraph',
			text: 'Om du har köpt pass för ditt företag enligt avsnitt 4 kan du be oss att visa företaget med dess namn och logotyp i stället för ett namn och en avatar. Om ditt momsregistreringsnummer kommer från ett annat EU-land än Sverige måste VIES ha bekräftat det.',
		},
		{
			kind: 'paragraph',
			text: 'Mejla oss via supportvägen på vår [sida med företagsinformation](/company-information) från mejladressen till det Fluxer-konto som har passen, och skicka företagets namn och en kvadratisk logotyp. Vi kan be dig visa att du får företräda företaget.',
		},
		{
			kind: 'paragraph',
			text: 'Företagsplatser visas tillsammans i en egen del av väggen, efter platserna för personer. Alla företagsplatser har samma storlek, och vi ordnar dem efter datumet för det första passet, tidigast först. Företagsplatser visas inte på vår sida om drift på egen server.',
		},
		{
			kind: 'paragraph',
			text: 'En företagsplats använder samma antal som en personlig plats. Pass som företagets konto har köpt och lämnat vidare till anställda räknas för både företagskontot och mottagarna, så länge varje konto har kvar minst ett aktivt pass.',
		},
		{
			kind: 'paragraph',
			text: 'Alla företagsplatser har fortfarande samma storlek. Ordningen följer regeln om leveransdatum i avsnitt 2.1. En företagsplats ersätter kontots egen plats, följer inte ändringar av kontots profil och har ingen länk.',
		},
		{
			kind: 'paragraph',
			text: 'En företagsplats visar bara att företaget har köpt pass. Den är ingen rekommendation och inget samarbete åt något håll, och den ger inte företaget något inflytande över vad vi bygger.',
		},
		{
			kind: 'paragraph',
			text: 'Avsnitt 2.1 gäller för företagets namn och logotyp på samma sätt som för ett namn och en avatar, bland annat vår granskning innan något visas, skälen till att vi kan avstå från att visa det och hur vi anpassar det till väggen. Vi kan också avstå från en företagsplats, eller avsluta en senare, om den kan få folk att tro att vi rekommenderar företaget eller samarbetar med det.',
		},
		{
			kind: 'paragraph',
			text: 'Exempel är företag inom spel om pengar, vuxeninnehåll, vapen, droger eller politiska kampanjer, eller företag som säljer följare, konton, recensioner, länkar, akademiska arbeten eller spam. Tills vi har godkänt en företagsplats, och när vi avstår från eller avslutar en, behåller kontot sin plats med ett namn och en avatar, och vi berättar varför.',
		},
		{
			kind: 'paragraph',
			text: 'Genom att skicka oss en logotyp bekräftar du att du får företräda företaget och att företaget har rätt att låta oss visa namnet och logotypen. Du låter oss visa dem kostnadsfritt och i hela världen på väggen, och på bilder och inspelningar av vår webbplats som visar väggen, medan företagsplatsen finns kvar. Det ger oss ingen annan rätt att använda dem.',
		},
		{
			kind: 'paragraph',
			text: 'Om någon meddelar oss att ett namn eller en logotyp på väggen gör intrång i deras rättigheter kan vi dölja det medan vi utreder saken. Platsen visar då det senaste namn och den senaste avatar som vi har godkänt, eller användarnamnet och standardavataren. Vi berättar vad vi har gjort och varför, och du kan svara eller överklaga som avsnitt 2.1 beskriver. Det är ingen borttagning av passets förmåner.',
		},
		{
			kind: 'paragraph',
			text: 'Om du vill byta logotyp eller gå tillbaka till ett namn och en avatar ber du oss på samma sätt. Du kan också lämna väggen när som helst, eller komma tillbaka till den, som avsnitt 2.1 beskriver.',
		},
		{
			kind: 'paragraph',
			text: 'Om du ändrar ditt namn eller din avatar för väggen i dina inställningar blir platsen en personlig plats igen när vi har godkänt ändringen. Om vi någon gång slutar visa företagsplatser går varje sådan plats tillbaka till kontots plats med ett namn och en avatar.',
		},
		{kind: 'heading', text: '3. Bonusen, och vad du inte får'},
		{
			kind: 'paragraph',
			text: 'Så länge vi väljer att erbjuda den får Operators också en bonus med de här fyra delarna.',
		},
		{
			kind: 'bullet',
			text: 'Rollen Operator i communityn Fluxer Labs. Den visar Operators som en egen grupp i medlemslistan och ger tillgång till den privata kanalen #operators, där Hampus Kraft, som skapade Fluxer, deltar när han kan. Där kan Operators prata om Fluxer, drift på egen server, API:et och botar och om en instans som de driver.',
		},
		{
			kind: 'bullet',
			text: 'Inbjudningar att testa nya funktioner innan de släpps. Funktioner som inte är släppta kan ha buggar. Prova dem där ett problem inte kan drabba dig eller dina användare.',
		},
		{
			kind: 'bullet',
			text: 'Ditt namn i årets lista "The Operators", till exempel "The Operators of 2026", om du väljer det i inställningarna. Vi publicerar den gratis i en taggad version av Fluxer.',
		},
		{
			kind: 'bullet',
			text: 'När vi har en lista över Fluxer-instanser på vår webbplats, en Operator-markering på listningen för en instans som du driver. Alla som följer reglerna för listan kan lägga till sin instans gratis, och det krävs aldrig något pass för att finnas med. Listan finns inte än, och vi kan ändra markeringen eller låta bli att erbjuda den.',
		},
		{
			kind: 'paragraph',
			text: 'Alla Operators får samma bonus, oavsett hur många pass de har. Bonusen ingår inte i det du betalar för, och de här villkoren ger dig ingen rätt till den.',
		},
		{
			kind: 'paragraph',
			text: 'Vi kan ändra, pausa eller avsluta en del av bonusen för alla när som helst, och då får du ingen återbetalning eller annan ersättning. När vi avslutar en del av bonusen för alla berättar vi det för Operators i Fluxer Labs eller via mejl. Vi tar bara bort en del av bonusen från en enskild person enligt avsnitt 8.',
		},
		{
			kind: 'paragraph',
			text: 'För att använda kanalen #operators och Operator-rollen behöver du vara medlem i communityt Fluxer Labs med det konto som har Operator Pass.',
		},
		{
			kind: 'paragraph',
			text: 'Kanalen är en plats för samtal, och passet är inget supportavtal. Det finns ingen garanterad svarstid eller servicenivå, och vi lovar inte att en fråga får svar, att Hampus deltar i ett visst samtal, att alla problem går att lösa eller att en funktion som du ber om blir byggd. Vi loggar inte in på, kör eller sköter någon instans som du driver, och du ansvarar själv för den och dess data.',
		},
		{kind: 'subheading', text: '3.1 Operators-listan i en version'},
		{
			kind: 'paragraph',
			text: 'Ditt namn kommer med i varje årlig lista som vi publicerar så länge du har passet, står på väggen och har "Lägg till mitt namn i den årliga Operators-listan" på i dina inställningar. Inställningen är av tills du slår på den. Listan använder ditt namn på väggen vid den tidpunkten och kommer med i versionsanteckningarna och i en fil i Fluxers källkodsförråd.',
		},
		{
			kind: 'paragraph',
			text: 'Om du ber oss radera ditt namn efter att en lista har publicerats utelämnar vi det i alla kommande versioner, redigerar de publicerade versionsanteckningarna så att det försvinner och tar bort det ur listfilen i nästa ändring av källkoden. Äldre versioner i källkodens historik och kopior som andra har gjort går inte att kalla tillbaka.',
		},
		{kind: 'subheading', text: '3.2 Vad passet inte ger'},
		{
			kind: 'paragraph',
			text: 'Passet låser inte upp något i Fluxer-programvaran. Fluxer som du driver själv fungerar likadant med eller utan passet och förblir gratis att driva, och om du förlorar passet påverkas aldrig en instans som du driver. Passet ger inga Plutonium-förmåner, ingen hosting och ingen licens eller andra rättigheter till programvaran. Passet har inget kontantvärde.',
		},
		{kind: 'heading', text: '4. Pris och betalning'},
		{
			kind: 'paragraph',
			text: 'Priset för ett pass visas på [sidan om Operator Pass](/pass) och i kassan innan du betalar, inklusive eventuell skatt som gäller. Du betalar en gång och inget förnyas, och avsnitt 4.1 i [användarvillkoren](/terms#4-1-payments-and-renewal) anger hur betalningen går till. Om priset ändras efter ditt köp tar vi inte ut mer av dig, och vi betalar inte heller tillbaka mellanskillnaden.',
		},
		{
			kind: 'paragraph',
			text: 'I kassan kan du också lägga till en månatlig donation. Den är skild från passet, de här villkoren omfattar den inte, och du kan avsluta den när som helst på [donationssidan](/donate/manage).',
		},
		{
			kind: 'paragraph',
			text: 'Om du köper för ditt företag kryssar du i "Jag köper för ett företag" i kassan för att bekräfta att du köper passen för din näringsverksamhet eller ditt yrke. Kassan ber dig då om företagets namn, adress och momsregistreringsnummer, och du får en faktura med dem. Du betalar samma totalpris som alla andra köpare.',
		},
		{
			kind: 'paragraph',
			text: 'Om ditt momsregistreringsnummer kommer från ett annat EU-land än Sverige gäller omvänd betalningsskyldighet för ditt köp, och du redovisar själv momsen, som det står på fakturan. Ett företag i Sverige betalar svensk moms, som redan ingår i priset. Utanför EU ingår också den skatt som gäller i priset, och den står på fakturan.',
		},
		{
			kind: 'paragraph',
			text: 'Vi kontrollerar momsregistreringsnummer från EU-länder i Europeiska kommissionens tjänst VIES efter att du har betalat. Om VIES inte bekräftar ditt nummer är totalpriset du betalade detsamma, du har ändå köpt för ditt företag, och bara vår momsredovisning påverkas.',
		},
		{
			kind: 'paragraph',
			text: 'Dina pass går till det Fluxer-konto som du köper med. Momsregistreringsnumret står bara på din faktura. Företagets namn visas på väggen bara om du väljer det som ditt namn på väggen eller ber om en företagsplats enligt [avsnitt 2.2](/operator-pass-terms#2-2-listing-your-business), och aldrig på ditt märke.',
		},
		{
			kind: 'paragraph',
			text: 'Du kan lämna vidare varje pass till en anställds konto enligt avsnitt 11 när ni är vänner på Fluxer, och att lämna vidare pass ändrar inte din faktura.',
		},
		{kind: 'heading', text: '5. Hur länge dina förmåner gäller'},
		{
			kind: 'paragraph',
			text: 'Du behåller dina förmåner "för livet". I de här villkoren betyder för livet så länge vi driver den officiella Fluxer-tjänsten och webbplatsen, aldrig en persons livstid.',
		},
		{
			kind: 'paragraph',
			text: 'Under den tiden lovar vi att inte ta bort märket eller din plats på väggen, om inte passet upphör enligt avsnitt 8, 9 eller 10, du lämnar vidare ditt enda pass enligt avsnitt 11 eller du själv väljer att dölja dem. Dina förmåner beror inte på hur ofta du använder Fluxer, och vi raderar aldrig ett konto som har passet på grund av inaktivitet.',
		},
		{
			kind: 'paragraph',
			text: 'Om Fluxer Platform AB eller den officiella Fluxer-tjänsten säljs eller överlåts till någon annan måste den nya ägaren ta över alla befintliga pass på samma villkor.',
		},
		{kind: 'heading', text: '6. Ändringar av dina förmåner'},
		{
			kind: 'paragraph',
			text: 'Vi kan ändra hur märket och väggen ser ut, var väggen finns på startsidan och listans ordning och utformning, så länge din plats fortfarande visar ditt namn och din avatar, eller företagets namn och logotyp för en företagsplats som avsnitt 2.2 beskriver, och märket finns kvar på din profil. Ändringar kostar dig ingenting.',
		},
		{
			kind: 'paragraph',
			text: 'Om vi bestämmer oss för att lägga ner den officiella Fluxer-tjänsten och webbplatsen berättar vi det för Operators via mejl minst 30 dagar i förväg. Om lagen eller Fluxers säkerhet gör att vi måste ta ner märket eller väggen medan vi fortfarande driver den officiella tjänsten, berättar vi det för dig så tidigt vi kan. Inget i det här avsnittet begränsar de rättigheter som lagen ger dig, som avsnitt 7.3 beskriver.',
		},
		{kind: 'heading', text: '7. Återbetalningar och ångerrätt'},
		{kind: 'subheading', text: '7.1 Ångerrätt'},
		{
			kind: 'paragraph',
			text: 'I EU, EES, Storbritannien och vissa andra länder kan konsumenter ångra ett köp på nätet inom en viss tidsperiod utan att ange något skäl. För digitalt innehåll upphör den rätten när leveransen har påbörjats, men bara om du bad om leverans under ångerfristen och godtog att du skulle förlora rätten.',
		},
		{
			kind: 'paragraph',
			text: 'Innan du betalar måste varje köpare, oavsett om den köper som konsument eller för ett företag, kryssa i en ruta som aldrig är ikryssad i förväg, och du kan inte köpa ett pass utan att kryssa i den rutan. När du kryssar i den ber du oss leverera varje pass i ditt köp så fort du har betalat, och du bekräftar att du förstår att du förlorar din rätt att ångra köpet när de har levererats och att bonusen inte ingår i priset.',
		},
		{
			kind: 'paragraph',
			text: 'Innan vi levererar mejlar vi dig en bekräftelse som upprepar det du godkände, med tidpunkten för godkännandet, priset och de här villkoren. Vi sparar ditt samtycke, tidpunkten, versionen av de här villkoren, din IP-adress, uppgifter om din webbläsare och ditt konto.',
		},
		{
			kind: 'paragraph',
			text: 'När ett pass har levererats kan du inte ångra köpet av det. Om vi inte har levererat det än, till exempel på grund av ett tekniskt fel, kan du fortfarande frånträda avtalet med "Ångra avtalet här" bredvid köpet i dina inställningar eller genom att meddela oss på något annat tydligt sätt via supportvägen på vår [sida med företagsinformation](/company-information).',
		},
		{
			kind: 'paragraph',
			text: 'Vi betalar då tillbaka hela beloppet inom 14 dagar, med samma betalningsmetod, och om köpet kom med en faktura skickar vi också en kreditfaktura som hänvisar till den.',
		},
		{
			kind: 'paragraph',
			text: 'Om en domstol eller en konsumentmyndighet där du bor beslutar att du fortfarande har ångerrätt efter leveransen följer vi det beslutet och betalar tillbaka hela beloppet.',
		},
		{
			kind: 'paragraph',
			text: 'Den som tar emot ett pass enligt avsnitt 11 betalar inte för det och har ingen ångerrätt. Om du köper ett pass för ditt företag har du ingen ångerrätt enligt det här avsnittet, även om VIES inte bekräftar ditt momsregistreringsnummer. Avsnitt 12 lägger till det som gäller i vissa länder, och där det står så gäller det även köp för företag.',
		},
		{kind: 'subheading', text: '7.2 Formulär för att ångra köpet'},
		{
			kind: 'paragraph',
			text: 'Så länge du har ångerrätt kan du kopiera formuläret, fylla i det och skicka det till <support@fluxer.com>. Du behöver inte använda det.',
		},
		{
			kind: 'quote',
			text: 'Till Fluxer Platform AB, c/o Embassy House, Östgötagatan 12, 116 25 Stockholm, Sverige, support@fluxer.com. Jag ångrar mitt avtal om Operator Pass. Köpt den (datum). Namn. Fluxer-kontots mejladress. Datum.',
		},
		{kind: 'subheading', text: '7.3 Efter leveransen'},
		{
			kind: 'paragraph',
			text: 'Utom när lagen där du bor ger dig en rätt som inte går att avstå från är köp slutgiltiga och vi betalar inte tillbaka dem, även om vi kan välja att göra det. För passet ersätter det här avsnittet avsnitt 4.2 och 4.3 i [användarvillkoren](/terms#4-2-refunds).',
		},
		{
			kind: 'paragraph',
			text: 'Dina rättigheter enligt lag påverkas inte. Du får fortfarande pengarna tillbaka eller en annan påföljd när lagen ger dig det, till exempel när vi debiterar fel belopp eller mer än en gång, när någon har använt din betalningsmetod utan ditt tillstånd, eller när dina förmåner inte levereras eller inte är som de beskrivs här eller på sidan om passet.',
		},
		{kind: 'heading', text: '8. När vi tar bort passets förmåner'},
		{
			kind: 'paragraph',
			text: 'I det här avsnittet och avsnitt 9 betyder "passets förmåner" dina förmåner och din tillgång till bonusen. Vi kan ta bort några eller alla av passets förmåner, under en begränsad tid eller permanent, om du bryter mot de här villkoren, [användarvillkoren](/terms) eller [communityriktlinjerna](/guidelines) någonstans i den officiella Fluxer-tjänsten, även i Fluxer Labs.',
		},
		{
			kind: 'paragraph',
			text: 'Det omfattar att missbruka #operators eller rollen Operator, som att trakassera teamet eller andra Operators, sprida det andra skriver där utan deras tillstånd, göra reklam eller ge någon annan tillgång till kanalen. Det omfattar också att använda din roll eller ditt märke för att se ner på andra eller agera som om du hade makt över dem.',
		},
		{
			kind: 'paragraph',
			text: 'Vi bedömer en överträdelse efter de regler som gällde när den skedde.',
		},
		{
			kind: 'paragraph',
			text: 'Vid en mindre överträdelse börjar vi med en varning eller med att ta bort passets förmåner under en begränsad tid, som avsnitt 9 i [communityriktlinjerna](/guidelines#9-åtgärder-och-överklaganden) beskriver. Vi tar bara bort passet permanent vid en allvarlig överträdelse, som hot, trakasserier, olagligt innehåll eller bedrägeri, eller vid en överträdelse efter att vi redan har tagit bort passets förmåner under en begränsad tid.',
		},
		{
			kind: 'paragraph',
			text: 'Innan vi tar bort passet permanent berättar vi varför och ger dig 7 dagar att svara, om inte överträdelsen är så allvarlig att vi måste agera direkt.',
		},
		{
			kind: 'paragraph',
			text: 'Om vi avslutar ditt Fluxer-konto för att du har brutit mot våra villkor eller communityriktlinjerna upphör passet samtidigt. Vi tar bara bort dig från Operators-väggen vid en allvarlig överträdelse eller vid upprepade överträdelser, och aldrig bara för att vi har avstått från att visa ett namn eller en avatar enligt [avsnitt 2.1](/operator-pass-terms#2-1-your-name-and-avatar). Vi tar bara bort passets förmåner av de skäl som står i de här villkoren.',
		},
		{
			kind: 'paragraph',
			text: 'När vi tar bort passets förmåner får du den motivering som avsnitt 6.2 i [användarvillkoren](/terms#6-2-enforcement-by-fluxer) beskriver, inklusive hur länge borttagningen kommer att gälla, och du kan överklaga enligt det avsnittet, även via vägen till tvistlösning utanför domstol som vår [hjälpsida om tvister enligt DSA](/help/dsa-dispute-resolution) beskriver. Förmåner som vi tar bort betalas inte tillbaka.',
		},
		{
			kind: 'paragraph',
			text: 'Om vi tar bort passet permanent kan du inte köpa ett pass eller ta emot ett igen. Att vi tar bort passets förmåner stänger inte av ditt Fluxer-konto i sig, och en åtgärd mot ditt konto följer avsnitt 6.2 i användarvillkoren.',
		},
		{
			kind: 'paragraph',
			text: 'Om överklagandet visar att vi gjorde fel får du tillbaka passets förmåner, med det ursprungliga datumet på ditt märke. Du får också tillbaka dem om en åtgärd mot ditt Fluxer-konto som skedde tillsammans med borttagningen upphävs efter ett överklagande.',
		},
		{
			kind: 'paragraph',
			text: 'Som konsument kan du också ta en tvist om en borttagning till Allmänna reklamationsnämnden, som vår [sida med företagsinformation](/company-information#konsumenttvister) beskriver.',
		},
		{kind: 'heading', text: '9. Betalningstvister och återkrav'},
		{
			kind: 'paragraph',
			text: 'Du kan alltid bestrida en debitering via din bank eller ditt kortföretag, och inget i de här villkoren hindrar dig. Om något är fel med en betalning för passet går det snabbast att meddela oss via supportvägen på vår [sida med företagsinformation](/company-information).',
		},
		{
			kind: 'paragraph',
			text: 'Medan ett bestridande av någon betalning som du har gjort till oss pågår kan du inte köpa något pass. Medan ett bestridande av en betalning för passet pågår kan du inte heller köpa något annat på Fluxer, och vi pausar passet. Du kan köpa igen så snart bestridandet har avslutats, och vi kan be dig om uppgifter för att kunna svara på det.',
		},
		{
			kind: 'paragraph',
			text: 'När vi anser att ett bestridande är fel bestrider vi det och visar din bank uppgifterna om ditt samtycke i avsnitt 7.1, de företagsuppgifter du lämnade i kassan om du köpte för ditt företag, bekräftelsemejlet och när vi levererade passet.',
		},
		{
			kind: 'paragraph',
			text: 'Om bestridandet avslutas utan att pengarna går tillbaka till dig, även när du drar tillbaka det, återställer vi passet och ingenting händer med ditt konto.',
		},
		{
			kind: 'paragraph',
			text: 'Om det slutar med att pengarna går tillbaka till dig upphör varje pass som betalningen avsåg, och vi räknar bort det. För ett pass som har lämnats vidare gäller det den som har passet, som avsnitt 11 beskriver.',
		},
		{
			kind: 'paragraph',
			text: 'Pass som andra betalningar avsåg påverkas inte. När det sista passet på ett konto upphör tar vi bort kontots märke och plats på väggen. För ett pass som har lämnats vidare förlorar även köparen tillgodoräknandet för det.',
		},
		{
			kind: 'paragraph',
			text: 'En person i vårt team prövar varje bestridande av en betalning för passet medan det pågår. Ett bestridande är obefogat bara när du själv eller någon du har gett lov gjorde betalningen, vi debiterade rätt belopp, vi levererade passet som de här villkoren och sidan om passet beskriver, vi inte hade betalat tillbaka beloppet och din ångerrätt enligt [avsnitt 7.1](/operator-pass-terms#7-1-withdrawal) eller avsnitt 12 hade upphört när du bestred.',
		},
		{
			kind: 'paragraph',
			text: 'Ett bestridande är alltid befogat, och leder aldrig till någon åtgärd mot ditt Fluxer-konto utöver pausen som beskrivs ovan, i vart och ett av de här fallen.',
		},
		{kind: 'bullet', text: 'Någon har använt din betalningsmetod utan ditt tillstånd.'},
		{kind: 'bullet', text: 'Vi debiterade fel belopp eller mer än en gång.'},
		{kind: 'bullet', text: 'Vi hade redan betalat tillbaka beloppet.'},
		{kind: 'bullet', text: 'Vi levererade inte passet, eller det var inte som det beskrivs.'},
		{kind: 'bullet', text: 'Du bestred medan du fortfarande hade ångerrätt.'},
		{kind: 'bullet', text: 'Du ångrade köpet enligt avsnitt 7.1 eller avsnitt 12 och vi betalade inte tillbaka.'},
		{
			kind: 'bullet',
			text: 'Du bestrider en borttagning enligt [avsnitt 8](/operator-pass-terms#8-when-we-remove-perks) som du anser var fel.',
		},
		{
			kind: 'bullet',
			text: 'Din bank eller ditt kortföretag anger som grund att betalningen inte gjordes av dig eller någon du har gett lov, att vi debiterade fel belopp eller mer än en gång, att vi inte betalade tillbaka ett belopp vi var skyldiga dig, eller att passet inte levererades som det beskrivs.',
		},
		{
			kind: 'paragraph',
			text: 'Om vi bedömer att ett bestridande är obefogat meddelar vi dig det via mejl och i ett meddelande i Fluxer, med våra skäl och ett datum några dagar innan vi måste svara din bank. Om bestridandet inte har dragits tillbaka före det datumet och det slutar med att pengarna går tillbaka till dig raderar vi permanent det Fluxer-konto som gjorde betalningen, och det går inte att återställa. Varken en inloggning eller en begäran till supporten stoppar raderingen.',
		},
		{
			kind: 'paragraph',
			text: 'Från det att vi stänger kontot av det här skälet får du inte skapa eller använda andra Fluxer-konton, och vi spärrar dess e-postadress från att registreras igen.',
		},
		{
			kind: 'paragraph',
			text: 'Vi raderar kontot 60 dagar efter att bestridandet har avslutats, och fram till dess kan du fortfarande begära en kopia av dina uppgifter, som avsnitt 10 i [integritetspolicyn](/privacy#10-your-rights) beskriver.',
		},
		{
			kind: 'paragraph',
			text: 'Du kan överklaga enligt avsnitt 6.2 i [användarvillkoren](/terms#6-2-enforcement-by-fluxer), och om överklagandet visar att bestridandet var befogat avbryter vi raderingen och häver spärren.',
		},
		{
			kind: 'paragraph',
			text: 'Om vår betalleverantör varnar oss för att en betalning för passet sannolikt var bedräglig betalar vi tillbaka den, rapporterar den till leverantören och spärrar de betalningsuppgifter som användes, och passet som betalningen avsåg upphör. Varningen i sig leder aldrig till någon åtgärd mot ditt konto.',
		},
		{kind: 'heading', text: '10. Om du raderar ditt konto'},
		{
			kind: 'paragraph',
			text: 'Ett pass hör till det konto som har det. Om du raderar kontot upphör varje pass på kontot när raderingen är klar efter respitperioden i avsnitt 6.1 i [användarvillkoren](/terms#6-1-disabling-or-deleting-your-account), ett erbjudande som du har gjort enligt avsnitt 11 dras tillbaka, och vi tar ner din plats på väggen. Pass betalas inte tillbaka när du raderar kontot. Om du vill att ett pass som du inte har lämnat vidare än ska finnas kvar kan du lämna vidare det enligt avsnitt 11 innan raderingen är klar.',
		},
		{kind: 'heading', text: '11. Lämna vidare'},
		{
			kind: 'paragraph',
			text: 'Du kan erbjuda ett pass som du har köpt till ett annat Fluxer-konto. Det kontot måste vara din vän på Fluxer både när du erbjuder passet och när mottagaren godkänner det. Varje pass kan lämnas vidare bara en gång, och ett pass som du har fått av någon annan kan inte lämnas vidare igen. Ett pass som ingår i Visionary kan inte lämnas vidare alls, och ett pass kan inte gå till ett konto som redan har förlorat passet enligt avsnitt 8.',
		},
		{
			kind: 'paragraph',
			text: 'Tills mottagaren godkänner är passet fortfarande ditt och räknas fortfarande för dig, och du kan ta tillbaka erbjudandet. För att godkänna det godkänner mottagaren de här villkoren och väljer sitt namn för väggen, eller väljer att inte stå med. Därefter flyttas passet till mottagarens konto och vi levererar det.',
		},
		{
			kind: 'paragraph',
			text: 'Mottagarens märke slås på och mottagarens plats kommer upp på väggen, eller så ökar antalet om mottagaren redan har ett pass. Från och med då gäller de här villkoren för mottagaren för det passet.',
		},
		{
			kind: 'paragraph',
			text: 'Efter leveransen räknas passet fortfarande för dig som köpare om du har kvar minst ett aktivt pass. Det räknas också för mottagaren, från det datum då vi levererar det till mottagaren. Det ger dig ingen äganderätt till eller kontroll över passet som du har lämnat vidare.',
		},
		{
			kind: 'paragraph',
			text: 'Ett pass flyttas först när 72 timmar har gått sedan betalningen för det, och bara om betalningen inte har någon varning om bedrägeri, något pågående bestridande eller någon återbetalning. Avsnitt 12 anger en längre väntetid i vissa länder. Fram till dess ser mottagaren när passet kommer fram.',
		},
		{
			kind: 'paragraph',
			text: 'Om du lämnar vidare ditt enda kvarvarande pass försvinner ditt Operator-märke och din plats på väggen, och vi ber dig bekräfta innan det sker. Tillgodoräknade pass som du har köpt håller inte dessa förmåner aktiva på egen hand.',
		},
		{
			kind: 'paragraph',
			text: 'Om du senare har ett aktivt pass igen omfattar ditt antal även pass som du har köpt och lämnat vidare och som fortfarande är giltiga.',
		},
		{
			kind: 'paragraph',
			text: 'Att lämna vidare ett pass är ingen försäljning. Du får ingen återbetalning eller ersättning för det, och mottagaren betalar ingenting. Köpet förblir ditt. En återbetalning som lagen eller de här villkoren ger för köpet går till dig, med den betalningsmetod du använde. Om betalningen för ett pass betalas tillbaka eller går tillbaka till dig upphör det passet för den som har det, som avsnitt 9 beskriver. Avsnitt 2, 5, 6, 7.3, 8 och 10 gäller för den som har ett pass som har lämnats vidare som om den personen hade köpt det.',
		},
		{
			kind: 'paragraph',
			text: 'Den personen har ingen egen ångerrätt, som avsnitt 7.1 anger, och kan i stället tacka nej till erbjudandet. När ett pass har flyttats till ett annat konto hör det till det kontot. Ett företag som lämnar vidare ett pass till en anställd kan inte ta tillbaka det, och passet stannar hos personen om den slutar på företaget.',
		},
		{kind: 'heading', text: '12. Regler för vissa länder'},
		{
			kind: 'paragraph',
			text: 'Vissa länder ger konsumenter en rätt att ångra ett köp på nätet som inte går att avstå från, även efter leveransen. Där det här avsnittet nämner ett sådant land gäller den rätten i stället för avståendet i avsnitt 7.1.',
		},
		{
			kind: 'paragraph',
			text: '**Brasilien.** Om din faktureringsadress eller ditt kort är från Brasilien, eller om du betalar i brasilianska real, kan du ångra köpet inom 7 dagar från betalningen, oavsett om du köper som konsument eller för ditt företag, och vi levererar ändå dina pass till dig direkt. För att ångra köpet använder du "Avbryt köpet" bredvid passet i dina inställningar, eller meddelar oss via supportvägen som finns på vår [sida med företagsinformation](/company-information).',
		},
		{
			kind: 'paragraph',
			text: 'Vi bekräftar direkt att du har ångrat köpet, betalar genast tillbaka hela priset för det passet, med samma betalningsmetod eller som en återbetalning via Pix, och passet upphör. Om köpet kom med en faktura skickar vi också en kreditfaktura som hänvisar till den. Ett pass som har köpts på det sättet kan lämnas vidare enligt avsnitt 11 först när de 7 dagarna har gått.',
		},
		{
			kind: 'paragraph',
			text: '**Australien.** Om du är konsument i Australien innebär avsnitt 7.3 att vi inte betalar tillbaka om du ångrar dig. Våra förmåner omfattas av garantier som inte kan uteslutas enligt Australian Consumer Law, och du har rätt till återbetalning vid ett väsentligt fel.',
		},
		{kind: 'heading', text: '13. Visionary'},
		{
			kind: 'paragraph',
			text: 'Visionary innehåller ett Operator Pass, och vi kommer att mejla alla med Visionary om att slå på passets förmåner. Tills du gör det gäller de här villkoren inte för dig, och Visionary förblir som det är.',
		},
		{kind: 'heading', text: '14. Ändringar och kontakt'},
		{
			kind: 'paragraph',
			text: 'Vi ändrar de här villkoren på samma sätt som användarvillkoren, som avsnitt 9.1 i [användarvillkoren](/terms#9-1-changes-to-these-terms) beskriver, och för in varje ändring i vår [ändringslogg](/changelog). En ändring som gör det sämre för dig gäller för ett pass som du redan har bara om lagen kräver det eller om du godkänner den.',
		},
		{
			kind: 'paragraph',
			text: 'Om en översättning av de här villkoren skiljer sig från den engelska texten gäller den version som är mest förmånlig för dig. Alla kontaktvägar finns på vår [sida med företagsinformation](/company-information).',
		},
	],
} as const satisfies Record<string, ReadonlyArray<OperatorPassTermsBlock>>;
