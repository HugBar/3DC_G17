using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;
using DDDSample1.Infrastructure;
using DDDSample1.Infrastructure.Staffs;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.StaffData;
using DDDSample1.Domain.UserData;
using Microsoft.AspNetCore.Authentication.Cookies;
using Microsoft.AspNetCore.Authentication.Google;
using Microsoft.AspNetCore.Identity;
using Pomelo.EntityFrameworkCore.MySql.Infrastructure;
using System;
using DDDSample1.Infrastructure.Shared;
using System.Linq;
using Microsoft.Extensions.Logging;
using DDDSample1.Domain.PatientData;
using DDDSample1.Infrastructure.PatientData;
using DDDSample1.Domain.Auth;
using DDDSample1.Domain.User;
using DDDSample1.Domain.OperationRequestData;
using DDDSample1.Infrastructure.OperationRequestData;
using System.Threading.Tasks;
using System.Text;
using System.Security.Claims;
using Microsoft.IdentityModel.Tokens;
using DDDSample1.Domain.OperationTypeData;
using DDDSample1.Infrastructure.OperationTypeData;
using System.Text.Json.Serialization;
using System.Text.Json;

namespace DDDSample1
{
    public class Startup
    {
        public Startup(IConfiguration configuration)
        {
            Configuration = configuration;
        }

        public IConfiguration Configuration { get; }

        public void ConfigureServices(IServiceCollection services)
        {
            // Connection string from appsettings.json
            var connectionString = Configuration.GetConnectionString("DefaultConnection");

            // Configure MySQL Database
            services.AddDbContext<DDDSample1DbContext>(opt =>
                opt.UseMySql(connectionString, ServerVersion.AutoDetect(connectionString))
                   .ReplaceService<IValueConverterSelector, StronglyEntityIdValueConverterSelector>());

            services.Configure<IdentityOptions>(options =>
            {
                // Account lockout settings
                options.Lockout.DefaultLockoutTimeSpan = TimeSpan.FromMinutes(15);
                options.Lockout.MaxFailedAccessAttempts = 5;
                options.Lockout.AllowedForNewUsers = true;
            });

            services.AddCors(options =>
   {
       options.AddPolicy("AllowAllOrigins",
           builder => builder.AllowAnyOrigin()
                             .AllowAnyMethod()
                             .AllowAnyHeader());
   });

            // Configure Identity with ApplicationUser
            services.AddIdentity<ApplicationUser, IdentityRole>()
                .AddEntityFrameworkStores<DDDSample1DbContext>()
                .AddDefaultTokenProviders();

            services.Configure<DataProtectionTokenProviderOptions>(options =>
            {
                options.TokenLifespan = TimeSpan.FromHours(24);
            });

            services.Configure<IdentityOptions>(options =>
            {
                options.Password.RequireDigit = true;
                options.Password.RequireLowercase = true;
                options.Password.RequireNonAlphanumeric = true;
                options.Password.RequireUppercase = true;
                options.Password.RequiredLength = 8;
                options.Tokens.PasswordResetTokenProvider = TokenOptions.DefaultProvider;
            });


            // Configure Authentication using Google
            services.AddAuthentication(options =>
            {
                options.DefaultAuthenticateScheme = CookieAuthenticationDefaults.AuthenticationScheme;
                options.DefaultChallengeScheme = GoogleDefaults.AuthenticationScheme;
            })


            .AddJwtBearer(options =>
            {
                options.TokenValidationParameters = new TokenValidationParameters
                {
                    ValidateIssuer = true,
                    ValidateAudience = true,
                    ValidateLifetime = true,
                    ValidateIssuerSigningKey = true,
                    ValidIssuer = Configuration["Jwt:Issuer"],
                    ValidAudience = Configuration["Jwt:Audience"],
                    IssuerSigningKey = new SymmetricSecurityKey(Encoding.UTF8.GetBytes(Configuration["Jwt:Key"])),
                    RoleClaimType = ClaimTypes.Role  // Ensure role claims are checked
                };
            })

            .AddCookie()
            .AddGoogle(googleOptions =>
            {
                googleOptions.ClientId = Configuration["Authentication:Google:ClientId"];
                googleOptions.ClientSecret = Configuration["Authentication:Google:ClientSecret"];
                googleOptions.CallbackPath = "/signin-google";

                googleOptions.Scope.Add("openid");
                googleOptions.Scope.Add("profile");
                googleOptions.Scope.Add("email");
            });

            ConfigureMyServices(services);

            // Add Authorization

            services.AddAuthorization(options =>
            {
                options.AddPolicy("AdminPolicy", policy => policy.RequireRole("Admin"));
                options.AddPolicy("PatientPolicy", policy => policy.RequireRole("Patient"));
                options.AddPolicy("DoctorPolicy", policy => policy.RequireRole("Doctor"));
            });

            services.AddScoped<IEmailService, SmtpEmailService>();
            services.AddScoped<IAuthService, AuthService>();
            services.AddScoped<IUserService, UserService>();
            services.AddScoped<StaffService>();

            // Add this logging configuration
            services.AddLogging(logging =>
            {
                logging.ClearProviders();
                logging.SetMinimumLevel(LogLevel.Information);
                logging.AddConsole();
            });

            // Add Controllers with support for JSON
            services.AddControllers().AddNewtonsoftJson();
            services.AddControllers()
                .AddJsonOptions(options =>
                {
                    options.JsonSerializerOptions.PropertyNameCaseInsensitive = true;
                    options.JsonSerializerOptions.PropertyNamingPolicy = null;
                    options.JsonSerializerOptions.DefaultIgnoreCondition = JsonIgnoreCondition.Never;
                    options.JsonSerializerOptions.ReadCommentHandling = JsonCommentHandling.Disallow; // Desabilitar comentários no JSON
                    options.JsonSerializerOptions.AllowTrailingCommas = false; // Não permitir vírgulas extras

                    // Esta é a configuração chave para rejeitar propriedades desconhecidas
                    options.JsonSerializerOptions.UnknownTypeHandling = JsonUnknownTypeHandling.JsonNode;
                });

        }

        public void Configure(IApplicationBuilder app,
                              IWebHostEnvironment env,
                              UserManager<ApplicationUser> userManager,
                              RoleManager<IdentityRole> roleManager,
                              ILoggerFactory loggerFactory)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }
            else
            {
                app.UseHsts();
            }

            app.UseHttpsRedirection();

            app.UseRouting();

            app.UseCors("AllowAllOrigins");

            // Enable authentication and authorization
            app.UseAuthentication();
            app.UseAuthorization();

            app.UseEndpoints(endpoints =>
            {
                endpoints.MapControllers();
            });

            var logger = loggerFactory.CreateLogger<Startup>();
            logger.LogInformation("Application started");

            var serviceProvider = app.ApplicationServices;

            Task.Run(() => DataSeeder.SeedData(serviceProvider)).Wait();
        }

        public void ConfigureMyServices(IServiceCollection services)
        {
            services.AddTransient<IUnitOfWork, UnitOfWork>();

            services.AddTransient<IStaffRepository, StaffRepository>();
            services.AddTransient<StaffService>();

            services.AddTransient<IPatientRepository, PatientRepository>();
            services.AddTransient<PatientService>();

            services.AddTransient<ILoggingService, LoggingService>();

            services.AddScoped<IOperationRequestRepository, OperationRequestRepository>();
            services.AddScoped<OperationRequestService>();

            services.AddScoped<IOperationTypeRepository, OperationTypeRepository>();
            services.AddScoped<OperationTypeService>();

    

        }

    }
}
