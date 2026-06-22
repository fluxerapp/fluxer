// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_TR_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "{product_name} hesabın geçici olarak devre dışı bırakıldı",
		"body": "Merhaba {username},\n\nŞüpheli etkinlik tespit ettiğimiz için {product_name} hesabını geçici olarak devre dışı bıraktık.\n\n{reason, select,\n  null {}\n  other {Sebep: {reason}}\n}\n\nHesabına tekrar erişmek için şifreni sıfırlaman gerekiyor:\n\n{forgotUrl}\n\nŞifreni sıfırladıktan sonra tekrar giriş yapabilirsin.\n\nBunun bir hata olduğunu düşünüyorsan, lütfen destek ekibimizle iletişime geç.\n\n– {product_name} Güvenlik Ekibi"
	},
	"account_scheduled_deletion": {
		"subject": "{product_name} hesabın kalıcı olarak silinecek",
		"body": "Merhaba {username},\n\nHizmet Şartlarımızı veya Topluluk Kurallarımızı ihlal ettiğin için {product_name} hesabın kalıcı olarak silinmek üzere planlandı.\n\nPlanlanan silme tarihi: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Sebep: {reason}}\n}\n\nBu ciddi bir yaptırım eylemidir. Hesap verilerin planlanan tarihte kalıcı olarak silinecektir.\n\nLütfen incele:\n- Hizmet Şartları: {termsUrl}\n- Topluluk Kuralları: {guidelinesUrl}\n\nİtiraz süreci:\nBu yaptırım kararının yanlış veya haksız olduğuna inanıyorsan, itiraz göndermek için 60 günün var. Bu e-posta adresinden {appeals_email} adresine e-posta gönder.\n\nİtirazında:\n- Yaptırım kararının neden yanlış veya haksız olduğuna inandığını açıkça açıkla\n- İlgili kanıt veya bağlam sağla\n\n{product_name} Güvenlik Ekibi'nden bir üye itirazını inceleyecek ve nihai bir karar verilene kadar bekleyen silme işlemini duraklatabilir.\n\n– {product_name} Güvenlik Ekibi"
	},
	"account_temp_banned": {
		"subject": "{product_name} hesabın geçici olarak askıya alındı",
		"body": "Merhaba {username},\n\nHizmet Şartlarımızı veya Topluluk Kurallarımızı ihlal ettiğin için {product_name} hesabını geçici olarak askıya aldık.\n\nSüre: {durationHours, plural,\n  =1 {1 saat}\n  other {# saat}\n}\nAskıya alınma bitiş tarihi: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Sebep: {reason}}\n}\n\nBu süre boyunca hesabına erişemeyeceksin.\n\nLütfen incele:\n- Hizmet Şartları: {termsUrl}\n- Topluluk Kuralları: {guidelinesUrl}\n\nBu yaptırım kararının yanlış veya haksız olduğuna inanıyorsan, itiraz gönderebilirsin. Bu e-posta adresinden {appeals_email} adresine e-posta gönder ve kararın neden yanlış olduğuna inandığını açıkça açıkla. İtirazını inceleyip kararımızı sana bildireceğiz.\n\n– {product_name} Güvenlik Ekibi"
	},
	"donation_confirmation": {
		"subject": "{product_name}'a yaptığın bağış için teşekkürler",
		"body": "Merhaba,\n\n{product_name}'a yaptığın bağış için teşekkürler! {interval, select,\n  month {tekrarlayan bağışın}\n  year {tekrarlayan bağışın}\n  other {tek seferlik bağışın}\n} başarıyla {interval, select,\n  month {ayarlandı}\n  year {ayarlandı}\n  other {işlendi}\n}.\n\nBağış detayları:\nMiktar: {amount} {currency} {interval, select,\n  month {aylık}\n  year {yıllık}\n  other {}\n}\n\nStripe, kısa süre içinde fatura PDF'ini içeren ayrı bir makbuzu sana e-posta ile gönderecektir. Bu, tüm ödeme detaylarını içerir ve vergi amaçlı kullanılabilir.\n\nBu bağlantıyı kullanarak istediğin zaman bağış geçmişini görüntüleyebilir, faturaları indirebilir, {interval, select,\n  month {ve aboneliğini yönetebilir veya iptal edebilirsin}\n  year {ve aboneliğini yönetebilir veya iptal edebilirsin}\n  other {ve gelecekteki bağışlarını yönetebilirsin}\n}:\n\n{manageUrl}\n\nDesteğin {product_name}'ın çalışmaya devam etmesine yardımcı oluyor. Teşekkürler!\n\n– {product_name} Ekibi"
	},
	"donation_magic_link": {
		"subject": "{product_name}'ın bağışlarını yönet",
		"body": "Merhaba,\n\nBağışçı portalına erişmek için aşağıdaki bağlantıya tıkla:\n\n{manageUrl}\n\nPortalda aboneliklerini yönetebilir, faturaları indirebilir ve bağış geçmişini görüntüleyebilirsin.\n\nBu bağlantı {expiresAt, date, full} {expiresAt, time, short} tarihinde sona erecektir.\n\nBu bağlantıyı sen talep etmediysen, bu e-postayı güvenle yok sayabilirsin.\n\n– {product_name} Ekibi"
	},
	"dsa_report_verification": {
		"subject": "DSA raporu için e-postanı doğrula",
		"body": "Merhaba,\n\n{product_name}'daki Dijital Hizmetler Yasası raporunu göndermek için aşağıdaki doğrulama kodunu kullan:\n\n{code}\n\nBu kod {expiresAt, date, full} {expiresAt, time, short} tarihinde sona erecektir.\n\nBunu sen talep etmediysen, bu e-postayı güvenle yok sayabilirsin.\n\n– {product_name} Güvenlik Ekibi"
	},
	"email_change_new": {
		"subject": "{product_name}'ın yeni e-postasını doğrula",
		"body": "Merhaba {username},\n\nYeni {product_name} e-postanı doğrulamak için bu kodu uygulamaya gir:\n\n{code}\n\nBu kod {expiresAt, date, full} {expiresAt, time, short} tarihinde sona erecektir.\n\nBunu sen talep etmediysen, bu e-postayı güvenle yok sayabilirsin.\n\n– {product_name} Ekibi"
	},
	"email_change_original": {
		"subject": "{product_name} e-posta değişikliğini onayla",
		"body": "Merhaba {username},\n\n{product_name} hesabındaki e-posta adresini değiştirme isteği aldık.\n\nBu değişikliği onaylamak için bu kodu uygulamaya gir:\n\n{code}\n\nBu kod {expiresAt, date, full} {expiresAt, time, short} tarihinde sona erecektir.\n\nBunu sen talep etmediysen, lütfen hemen hesabını güvence altına al.\n\n– {product_name} Ekibi"
	},
	"email_change_revert": {
		"subject": "{product_name} e-posta adresin değiştirildi",
		"body": "Merhaba {username},\n\n{product_name} hesabındaki e-posta adresi {newEmail} olarak değiştirildi.\n\nBu değişikliği sen yaptıysan, herhangi bir işlem yapmana gerek yok. Yapmadıysan, bu bağlantıyı kullanarak değişikliği geri alabilir ve hesabını güvence altına alabilirsin:\n\n{revertUrl}\n\nBu işlem, önceki e-postanı geri yükleyecek, seni her yerden çıkış yapacak, bağlantılı telefon numaralarını kaldıracak, MFA'yı devre dışı bırakacak ve yeni bir şifre belirlemeni gerektirecektir.\n\n– {product_name} Güvenlik Ekibi"
	},
	"email_verification": {
		"subject": "{product_name} e-posta adresini doğrula",
		"body": "Merhaba {username},\n\n{product_name} hesabının e-posta adresini aşağıdaki bağlantıya tıklayarak doğrula:\n\n{verifyUrl}\n\nBir {product_name} hesabı oluşturmadıysan, bu e-postayı güvenle yok sayabilirsin.\n\nBu bağlantı 24 saat geçerlidir.\n\n– {product_name} Ekibi"
	},
	"gift_chargeback_notification": {
		"subject": "Kullandığın hediye avantajları kaldırıldı",
		"body": "Merhaba {username},\n\nKullandığın bir hediye kodunun ödemesi başlangıçta başka biri tarafından yapılmıştı. Bu ödeme daha sonra geri alındı (ters ibraz).\n\nBu nedenle, hediyeyi kullandığında hesabına eklenen avantajlar kaldırıldı.\n\nBunun bir hata olduğunu düşünüyorsan, lütfen destek ekibimizle iletişime geç ve hediye koduyla ilgili sahip olduğun tüm detayları ve ne zaman kullandığını belirt.\n\n– {product_name} Ekibi"
	},
	"harvest_completed": {
		"subject": "{product_name} veri dışa aktarımın indirilmeye hazır",
		"body": "Merhaba {username},\n\nVeri dışa aktarımın hazır.\n\nİndirme bağlantısı:\n{downloadUrl}\n\nDahil edilen mesajlar: {totalMessages, number}\nDosya boyutu: {fileSizeMB, number} MB\n\nBu bağlantı {expiresAt, date, full} {expiresAt, time, short} tarihinde sona erecektir.\n\nBu dışa aktarımı sen talep etmediysen, lütfen hemen şifreni değiştir ve destek ekibimizle iletişime geç.\n\n– {product_name} Ekibi"
	},
	"inactivity_warning": {
		"subject": "{product_name} hesabın hareketsizlik nedeniyle silinecek",
		"body": "Merhaba {username},\n\n{lastActiveDate, date, full} tarihinden beri {product_name} hesabında herhangi bir etkinlik görmedik.\n\n{deletionDate, date, full} {deletionDate, time, short} tarihine kadar giriş yapmazsan, hesabın hareketsizlik nedeniyle kalıcı olarak silinecektir.\n\nBuradan giriş yap:\n{loginUrl}\n\n{product_name}'ı yakın zamanda kullandıysan, lütfen hemen destek ekibimizle iletişime geç.\n\n– {product_name} Ekibi"
	},
	"ip_authorization": {
		"subject": "Yeni bir IP adresinden girişi yetkilendir",
		"body": "Merhaba {username},\n\n{product_name} hesabına yeni bir IP adresinden giriş denemesi tespit ettik:\n\nIP adresi: {ipAddress}\nKonum: {location}\n\nBu sen isen, lütfen aşağıdaki bağlantıya tıklayarak bu IP adresini yetkilendir:\n\n{authUrl}\n\nGiriş yapmaya sen çalışmadıysan, lütfen hemen şifreni değiştir.\n\nBu bağlantı 30 dakika boyunca geçerlidir.\n\n– {product_name} Ekibi"
	},
	"password_change_verification": {
		"subject": "{product_name} şifre değişikliğini onayla",
		"body": "Merhaba {username},\n\n{product_name} hesabının şifresini değiştirme isteği aldık.\n\nBu değişikliği onaylamak için bu kodu uygulamaya gir:\n\n{code}\n\nBu kod {expiresAt} tarihinde sona erecektir.\n\nBunu sen talep etmediysen, birisi hesabına erişmiş olabilir. Şifreni hemen değiştir ve iki faktörlü kimlik doğrulamayı etkinleştir.\n\n– {product_name} Ekibi"
	},
	"password_reset": {
		"subject": "{product_name} şifreni sıfırla",
		"body": "Merhaba {username},\n\n{product_name} şifre sıfırlama talebinde bulundun. Yeni bir şifre belirlemek için aşağıdaki bağlantıyı kullan:\n\n{resetUrl}\n\nBunu sen talep etmediysen, bu e-postayı güvenle yok sayabilirsin.\n\nBu bağlantı 1 saat geçerlidir.\n\n– {product_name} Ekibi"
	},
	"registration_approved": {
		"subject": "{product_name} kaydın onaylandı",
		"body": "Merhaba {username},\n\nİyi haber: {product_name} kaydın onaylandı.\n\nArtık {product_name} uygulamasına buradan giriş yapabilirsin:\n{channelsUrl}\n\n{product_name} topluluğuna hoş geldin.\n\n– {product_name} Ekibi"
	},
	"report_resolved": {
		"subject": "{product_name} raporun incelendi",
		"body": "Merhaba {username},\n\nRaporun (Kimlik: {reportId}) Güvenlik Ekibimiz tarafından incelendi.{hasComment, select, yes {\n\nGüvenlik Ekibinden yanıt:\n{publicComment}} other {}}\n\n{product_name}'ı herkes için güvenli tutmaya yardımcı olduğun için teşekkür ederiz. Tüm raporları ciddiye alıyoruz ve topluluğa katkını takdir ediyoruz.\n\nBu sonuç hakkında herhangi bir sorunun veya endişen varsa, lütfen {safety_email} adresine ulaşabilirsin.\n\n– {product_name} Güvenlik Ekibi"
	},
	"scheduled_deletion_notification": {
		"subject": "{product_name} hesabın kalıcı olarak silinecek",
		"body": "Merhaba {username},\n\n{product_name} hesabın kalıcı olarak silinmek üzere planlandı.\n\nPlanlanan silme tarihi: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Sebep: {reason}}\n}\n\nBu ciddi bir yaptırım eylemidir. Hesap verilerin planlanan tarihte kalıcı olarak silinecektir.\n\nBu yaptırım kararının yanlış olduğuna inanıyorsan, itiraz gönderebilirsin. Bu e-posta adresinden {appeals_email} adresine e-posta gönder.\n\n– {product_name} Güvenlik Ekibi"
	},
	"self_deletion_scheduled": {
		"subject": "{product_name} hesabının silinmesi planlandı",
		"body": "Merhaba {username},\n\n{product_name} hesabını silme talebinde bulundun. Hesabın kalıcı olarak silinmek üzere planlandı:\n\n{deletionDate, date, full} {deletionDate, time, short} tarihinde\n\nBunu sen talep etmediysen, silme işlemini iptal etmek için hesabına giriş yap. Ayrıca hesabını güvence altına almak için şifreni değiştirmeni öneririz.\n\n– {product_name} Ekibi"
	},
	"unban_notification": {
		"subject": "{product_name} hesabının askıya alınması kaldırıldı",
		"body": "Merhaba {username},\n\nİyi haber: {product_name} hesabının askıya alınması kaldırıldı.\n\n{reason, select,\n  null {}\n  other {Sebep: {reason}}\n}\n\nArtık tekrar giriş yapabilir ve {product_name}'ı normal şekilde kullanmaya devam edebilirsin.\n\n– {product_name} Güvenlik Ekibi"
	}
});

export default EMAIL_I18N_TR_MESSAGES;
