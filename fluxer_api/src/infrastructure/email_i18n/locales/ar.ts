/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {EmailTranslations} from '../types';

export const ar: EmailTranslations = {
	passwordReset: {
		subject: 'إعادة تعيين كلمة مرور حسابك على Fluxer',
		body: `مرحباً {username}،

لقد طلبت إعادة تعيين كلمة المرور لحسابك على Fluxer. يرجى اتباع الرابط أدناه لتعيين كلمة مرور جديدة:

{resetUrl}

إذا لم تكن قد طلبت إعادة تعيين كلمة المرور، يمكنك تجاهل هذه الرسالة بأمان.

ستنتهي صلاحية هذا الرابط خلال ساعة واحدة.

- فريق Fluxer`,
	},
	emailVerification: {
		subject: 'تأكيد عنوان بريدك الإلكتروني على Fluxer',
		body: `مرحباً {username}،

يرجى تأكيد عنوان بريدك الإلكتروني لحسابك على Fluxer من خلال النقر على الرابط أدناه:

{verifyUrl}

إذا لم تقم بإنشاء حساب على Fluxer، يمكنك تجاهل هذه الرسالة بأمان.

ستنتهي صلاحية هذا الرابط خلال 24 ساعة.

- فريق Fluxer`,
	},
	ipAuthorization: {
		subject: 'السماح بتسجيل الدخول من عنوان IP جديد',
		body: `مرحباً {username}،

اكتشفنا محاولة تسجيل دخول إلى حسابك على Fluxer من عنوان IP جديد:

عنوان IP: {ipAddress}
الموقع: {location}

إذا كانت هذه المحاولة منك، يرجى السماح لهذا العنوان من خلال النقر على الرابط أدناه:

{authUrl}

إذا لم تحاول تسجيل الدخول، فيرجى تغيير كلمة مرورك فوراً.

ستنتهي صلاحية رابط التفويض هذا خلال 30 دقيقة.

- فريق Fluxer`,
	},
	accountDisabledSuspicious: {
		subject: 'تم تعطيل حسابك على Fluxer مؤقتاً',
		body: `مرحباً {username}،

تم تعطيل حسابك على Fluxer مؤقتاً بسبب نشاط مريب.

{reason, select,
	null {}
	other {السبب: {reason}

}}لاستعادة الوصول إلى حسابك، يجب عليك إعادة تعيين كلمة المرور:

{forgotUrl}

بعد إعادة تعيين كلمة المرور، ستتمكن من تسجيل الدخول مرة أخرى.

إذا كنت تعتقد أن هذا الإجراء تم عن طريق الخطأ، فيرجى التواصل مع فريق الدعم لدينا.

- فريق سلامة Fluxer`,
	},
	accountTempBanned: {
		subject: 'تم إيقاف حسابك على Fluxer مؤقتاً',
		body: `مرحباً {username}،

تم إيقاف حسابك على Fluxer مؤقتاً بسبب انتهاك شروط الخدمة أو إرشادات المجتمع.

المدة: {durationHours, plural,
	=1 {ساعة واحدة}
	other {# ساعات}
}
معلّق حتى: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {
السبب: {reason}}
}

خلال هذه الفترة، لن تتمكن من الوصول إلى حسابك.

نوصيك بمراجعة:
- شروط الخدمة: {termsUrl}
- إرشادات المجتمع: {guidelinesUrl}

إذا كنت تعتقد أن قرار الإنفاذ هذا غير صحيح أو غير مبرّر، يمكنك تقديم استئناف إلى appeals@fluxer.app من عنوان البريد الإلكتروني هذا. يرجى شرح سبب اعتقادك بأن القرار كان خاطئاً بوضوح. سنقوم بمراجعة الاستئناف والرد عليك بالقرار.

- فريق سلامة Fluxer`,
	},
	accountScheduledDeletion: {
		subject: 'تم جدولة حذف حسابك على Fluxer',
		body: `مرحباً {username}،

تم جدولة حسابك على Fluxer للحذف النهائي بسبب انتهاكات لشروط الخدمة أو إرشادات المجتمع.

تاريخ الحذف المجدول: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {
السبب: {reason}}
}

هذا إجراء إنفاذ جدي. سيتم حذف بيانات حسابك بشكل دائم في التاريخ المحدد.

نوصيك بمراجعة:
- شروط الخدمة: {termsUrl}
- إرشادات المجتمع: {guidelinesUrl}

عملية الاستئناف:
إذا كنت تعتقد أن قرار الإنفاذ هذا غير صحيح أو غير مبرّر، لديك 30 يوماً لتقديم استئناف إلى appeals@fluxer.app من عنوان البريد الإلكتروني هذا.

في استئنافك، يرجى:
- شرح سبب اعتقادك بأن قرار الإنفاذ غير صحيح أو غير مبرّر بشكل واضح
- تقديم أي أدلة أو سياق ذي صلة

سيقوم أحد أعضاء فريق سلامة Fluxer بمراجعة استئناك وقد يقوم بإيقاف الحذف المعلّق حتى يتم الوصول إلى قرار نهائي.

- فريق سلامة Fluxer`,
	},
	selfDeletionScheduled: {
		subject: 'تم جدولة حذف حسابك على Fluxer',
		body: `مرحباً {username}،

يحزننا أن نراك ترحل! تم جدولة حذف حسابك على Fluxer.

تاريخ الحذف المجدول: {deletionDate, date, full} {deletionDate, time, short}

مهم: يمكنك إلغاء عملية الحذف في أي وقت قبل {deletionDate, date, full} {deletionDate, time, short} بمجرد تسجيل الدخول إلى حسابك مرة أخرى.

قبل أن تغادر:
لوحة الخصوصية في إعدادات المستخدم تتيح لك:
- حذف رسائلك على المنصة
- استخراج أي بيانات مهمة قبل المغادرة

يرجى ملاحظة: بعد حذف حسابك، لن يكون من الممكن حذف رسائلك. إذا كنت ترغب في حذف رسائلك، يرجى القيام بذلك من خلال لوحة الخصوصية قبل إتمام حذف الحساب.

إذا غيّرت رأيك، فقط سجّل الدخول مرة أخرى لإلغاء الحذف.

- فريق Fluxer`,
	},
	inactivityWarning: {
		subject: 'سيتم حذف حسابك على Fluxer بسبب عدم النشاط',
		body: `مرحباً {username}،

لاحظنا أنك لم تسجّل الدخول إلى حسابك على Fluxer لمدة تزيد عن عامين.

آخر تسجيل دخول: {lastActiveDate, date, full} {lastActiveDate, time, short}

كجزء من سياسة الاحتفاظ بالبيانات لدينا، يتم تلقائياً جدولة حذف الحسابات غير النشطة. سيتم حذف حسابك بشكل دائم في التاريخ التالي:

تاريخ الحذف المجدول: {deletionDate, date, full} {deletionDate, time, short}

كيفية الحفاظ على حسابك:
يكفي أن تقوم بتسجيل الدخول إلى حسابك عبر {loginUrl} قبل تاريخ الحذف لإلغاء هذا الحذف التلقائي. لا يلزم اتخاذ أي إجراء آخر.

ماذا يحدث إذا لم تقم بتسجيل الدخول:
- سيتم حذف حسابك وجميع البيانات المرتبطة به بشكل دائم
- سيتم إرجاع رسائلك بشكل مجهول (منسوبة إلى "مستخدم محذوف")
- لا يمكن التراجع عن هذا الإجراء

هل تريد حذف رسائلك؟
إذا كنت ترغب في حذف رسائلك قبل حذف حسابك، يرجى تسجيل الدخول واستخدام لوحة الخصوصية في إعدادات المستخدم.

نأمل أن نراك مجدداً على Fluxer!

- فريق Fluxer`,
	},
	harvestCompleted: {
		subject: 'تصدير بياناتك من Fluxer جاهز للتنزيل',
		body: `مرحباً {username}،

تم الانتهاء من تصدير بياناتك وهو جاهز للتنزيل!

ملخص التصدير:
- إجمالي الرسائل: {totalMessages, number}
- حجم الملف: {fileSizeMB} ميغابايت
- الصيغة: ملف ZIP يحتوي على ملفات JSON

قم بتنزيل بياناتك: {downloadUrl}

مهم: سينتهي مفعول رابط التنزيل هذا في {expiresAt, date, full} {expiresAt, time, short}

ما الذي يتضمنه التصدير:
- جميع رسائلك منظّمة حسب القناة
- بيانات القنوات
- ملفك الشخصي ومعلومات حسابك
- عضويات الخوادم والإعدادات
- الجلسات الخاصة بالمصادقة ومعلومات الأمان

يتم تنظيم البيانات بصيغة JSON لتسهيل قراءتها وتحليلها.

إذا كانت لديك أي أسئلة حول تصدير بياناتك، فيرجى التواصل مع support@fluxer.app

- فريق Fluxer`,
	},
	unbanNotification: {
		subject: 'تم رفع إيقاف حسابك على Fluxer',
		body: `مرحباً {username}،

أخبار سارّة! تم رفع إيقاف حسابك على Fluxer.

السبب: {reason}

يمكنك الآن تسجيل الدخول إلى حسابك ومتابعة استخدام Fluxer.

- فريق سلامة Fluxer`,
	},
	scheduledDeletionNotification: {
		subject: 'تم جدولة حذف حسابك على Fluxer',
		body: `مرحباً {username}،

تم جدولة حسابك على Fluxer للحذف النهائي.

تاريخ الحذف المجدول: {deletionDate, date, full} {deletionDate, time, short}
السبب: {reason}

هذا إجراء إنفاذ جدي. سيتم حذف بيانات حسابك بشكل دائم في التاريخ المحدد.

إذا كنت تعتقد أن قرار الإنفاذ هذا غير صحيح، يمكنك تقديم استئناف إلى appeals@fluxer.app من عنوان البريد الإلكتروني هذا.

- فريق سلامة Fluxer`,
	},
	giftChargebackNotification: {
		subject: 'تم إلغاء هديّة Fluxer Premium الخاصة بك',
		body: `مرحباً {username}،

نود إبلاغك بأنه تم إلغاء هديّة Fluxer Premium التي قمت باستردادها بسبب نزاع دفع (استرجاع مبلغ) تم تقديمه من المشتري الأصلي.

تمت إزالة مزايا Premium من حسابك. تم اتخاذ هذا الإجراء لأن عملية الدفع الخاصة بالهدية تم الاعتراض عليها واسترجاعها.

إذا كانت لديك أي أسئلة بهذا الشأن، يرجى التواصل مع support@fluxer.app.

- فريق Fluxer`,
	},
	reportResolved: {
		subject: 'تمت مراجعة بلاغك على Fluxer',
		body: `مرحباً {username}،

تمت مراجعة بلاغك (المعرّف: {reportId}) من قبل فريق السلامة لدينا.

رد فريق السلامة:
{publicComment}

شكراً لمساهمتك في الحفاظ على Fluxer مكاناً آمناً للجميع. نحن نتعامل مع جميع البلاغات بجدية ونقدّر مساهمتك في مجتمعنا.

إذا كانت لديك أي أسئلة أو مخاوف بشأن هذه النتيجة، يرجى التواصل مع safety@fluxer.app.

- فريق سلامة Fluxer`,
	},
	dsaReportVerification: {
		subject: 'تحقق من بريدك الإلكتروني لبلاغ DSA',
		body: `مرحباً,

استخدم رمز التحقق التالي لتقديم بلاغك بموجب قانون الخدمات الرقمية على Fluxer:

{code}

تنتهي صلاحية هذا الرمز في {expiresAt, date, full} {expiresAt, time, short}.

إذا لم تطلب هذا، يرجى تجاهل هذه الرسالة.

- فريق سلامة Fluxer`,
	},
	registrationApproved: {
		subject: 'تمت الموافقة على تسجيلك في Fluxer',
		body: `مرحباً {username}،

أخبار رائعة! تمت الموافقة على تسجيلك في Fluxer.

يمكنك الآن تسجيل الدخول إلى تطبيق Fluxer عبر:
{channelsUrl}

مرحباً بك في مجتمع Fluxer!

- فريق Fluxer`,
	},
	emailChangeRevert: {
		subject: 'تم تغيير بريدك الإلكتروني في Fluxer',
		body: `مرحبًا {username},

تم تغيير بريد حسابك في Fluxer إلى {newEmail}.

إذا أجريت هذا التغيير، فلا حاجة لاتخاذ أي إجراء. إذا لم تفعل، يمكنك التراجع وحماية حسابك عبر هذا الرابط:

{revertUrl}

سيؤدي ذلك إلى استعادة بريدك السابق، وتسجيل خروجك من كل الجلسات، وإزالة أرقام الهواتف المرتبطة، وتعطيل MFA، وطلب كلمة مرور جديدة.

- فريق الأمان في Fluxer`,
	},
};
